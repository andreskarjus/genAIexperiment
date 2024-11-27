#####################
# This .R script file contains functions necessary to replicate the study
# Place this in the datapath folder and follow instructions in the main R script to source it.
# Author: Andres Karjus, 2024
#####################



get_prompts_openai = function(test=NULL, gptmodel="gpt-4o-2024-08-06", max_attempts=10, initial_delay=41, dfactor=1.5, promptcolumn="prompt", pathcolumn="filepath", temp=0, verbose=F, statss=T, visiondetail="high", nvariants=1L, maxwords=30, maxtokens=100L, imageinput=T){
  # sending grouped tibble here messes up
  # if data frame then takes and appends column, if vector then outputs vector
  
  start_time = Sys.time()
  
  #backoff and iterator
  i=1
  attempts=0
  delay = initial_delay
  reslist = vector("list", nrow(test))
  #params
  py$gptmodel=gptmodel
  py$temp = temp
  py$visiondetail = visiondetail # only for vision endpoint
  py$nvariants = nvariants
  py$maxtokens=maxtokens
  
  
  while(attempts <= max_attempts & i <= nrow(test)) {
    # prep wait time in case api fails:
    attempts = attempts + 1; delay = delay * dfactor
    tmp=NULL
    print(i)
    
    py$userprompt = test[i, promptcolumn,drop=T]
    tryCatch({
      if(verbose){ cat(paste0(" ", i, " "))}
      
      if(imageinput){
        py$base64_image = base64Encode(readBin(test[i, pathcolumn, drop=T], "raw", 
                                               file.info(test[i, pathcolumn, drop=T])[1, "size"]), "txt")
        py_run_string(
          '
tmpin = client.chat.completions.create(
  model=gptmodel,
  messages=[
    {
      "role": "user",
      "content": [
        {"type": "text", "text": userprompt},
        {
          "type": "image_url",
          "image_url": {
            "url": f"data:image/png;base64,{base64_image}",
            "detail": visiondetail
          },
        },
      ],
    }
  ],
  temperature=temp,
  n=nvariants,
  max_completion_tokens=maxtokens
)
del(base64_image) # cleanup
'
        )
    
      } else {
        py_run_string(
          '
tmpin = client.chat.completions.create(
  model=gptmodel,
  messages=[
    {
      "role": "user",
      "content": [
        {"type": "text", "text": userprompt}
      ],
    }
  ],
  temperature=temp,
  n=nvariants,
  max_completion_tokens=maxtokens
)
'
        )

      }
      
      
      # check if object exists, if API worked
      py_run_string(
        "
try:
  tmpin
except NameError:
  var_exists = False
else:
  var_exists = True
"
      )
      
      # if API bugs out, catches here, rest won't run:
      # If API does return something; throws catchable error if failed
      tmpin <<-py$tmpin # debug
      
      py_run_string(
        "
#res = tmpin.choices[0].message.content
res = [choice.message.content for choice in tmpin.choices] # iterate in case multiple outputs
completion_tokens = tmpin.usage.completion_tokens
prompt_tokens = tmpin.usage.prompt_tokens
total_tokens = tmpin.usage.total_tokens
")
      res = py$res  # new, parsed above; vector in case multiple
      # print(res)
      
      if (any(sapply(res, is.null))) {
        # Print a message for debugging
        print("NULL response encountered. Retrying or stopping")
        if(attempts>=max_attempts){
          stop("Out of attempts")
        } else {
          next  # retry with current iterator
        }
        
      }
      
      # if(!py$var_exists || res =="" | is.null(res) | is.na(res) ){
      #   stop("API didn't throw error but didn't give output either.")
      # }
      # runs only if API succeeded
      # tmp = cbind(test %>% slice(i),
      #              tibble(result = res
      #              )
      #  )
      
      tmp <- test %>%
        dplyr::slice(i) %>%  # always 1
        dplyr::slice(rep(1, length(res))) %>%  
        mutate(result = res)
      
      if(imageinput) {
        tmp=tmp %>% 
          mutate(
            nvariant= 1:length(res),
            expectedfile = paste(test$trial_number[i],
                                 test$trial_type[i], nvariant, sep="_") # %>% 
            #paste0(.,".png") # final imagevariant suffix added later still
          ) %>% 
          # constrain prompts to length if needed
          rowwise() %>% 
          mutate(nwords = strsplit(result, " ") %>% unlist %>% length) %>%
          mutate(imageprompt = case_when(nwords <=maxwords ~ result,
                                         nwords>maxwords ~ strsplit(result," ") %>% 
                                           unlist %>% .[1:maxwords] %>% paste(collapse=" ") )) %>% 
          ungroup()
      }
      
      if(statss){
        tmp = cbind(tmp,
                    tokenstotal=py$total_tokens,
                    tokensin=   py$prompt_tokens,
                    tokensout=  py$completion_tokens
        )
      }
      
      reslist[[i]] = tmp
      # cleanup
      rm(res, tmp)
      py_run_string(
        "
del(tmpin)
del(res)
")
      resdebug <<- reslist  # debug
      
      # ready to move on
      i=i+1
      attempts=0; delay = initial_delay # reset api waiting, as no backoff was necessary
      # Sys.sleep(0.01) # add tiny break
      # }
      
    }, error = function(e) {
      if (attempts >= max_attempts) {
        # Max attempts reached, re-throw the error
        print(paste("Returning partial list object, because max attempts reached. Error:", e))
        return(reslist)
      } else {
        print(paste(attempts, "attempt, waiting on API for", delay, "seconds because", e))
        Sys.sleep(delay)
      }
    })
  }
  
  if(verbose | statss){
    end_time = Sys.time()
    print(
      (difftime(end_time,start_time,  unit="secs")+as.POSIXct("1970-01-01", tz = "UTC")) %>% format( "%H:%M:%S")
    )
  }
  
  if(is.data.frame(test)){
    if(length(reslist)!=nrow(test)){
      warning("Nrow mismatch! Returning object, but likely some missing")
    }
    return(do.call(rbind, reslist))
    
  } else {
    x=unlist(reslist, F,F)  # assumes single output right now
    if(length(x)!=length(test)){
      warning("Length mismatch! Returning object, but likely misaligned")
    }
    return(x)
  }
}





getimages = function(indata, dp=datapath){
  
  ixlist = list()
  for(i in 1:nrow(indata)){
    
    ixdata = indata %>%
      dplyr::slice(i) %>%  # slice row to be duplicated
      dplyr::slice(rep(1, 4)) %>%  # Replicate the row 4 times
      mutate(imvariant = 1:4) %>% 
      mutate(newfile=
               file.path(dp, "airesults",
                         paste0(expectedfile,"_", imvariant, ".png") # imvariant matches idx below, very bad code
               ))
    ixlist[[i]]=ixdata
    
    py$newfile = file.path(dp, "airesults", ixdata$expectedfile[1]) # should match python below
    py$pr=ixdata$imageprompt[1]
    py$trial = ixdata$trial_type[1]
    py_run_string('# get the images
# Get the image URLs (assuming get_images is defined)
image_urls = get_images(prompt=pr)
#print(image_urls)

# Iterate over the image URLs and save each image
for idx, url in enumerate(image_urls, start=1):
    filename = newfile
    image_path = f"{newfile}_{idx}.png" # hardcoded 4 images, hacky
    print(image_path)
    response = requests.get(url)
    image = PilImage.open(BytesIO(response.content))
    image.show
    image.save(image_path)


# image_urls = get_images(prompt = pr)
# # get generated image
# for url in image_urls:
#     response = requests.get(url)
#     image = Image.open(BytesIO(response.content))
')
  }
  return(do.call(rbind, ixlist))
}


# Function to clean and rename files
rename_files <- function(file_path) {
  # Extract the filename without the folder path
  filename <- basename(file_path)
  
  # Remove the incorrect "_1.png" and the extra ".png"
  new_name <- str_replace(filename, "_1\\.png_(\\d+)\\.png$", "_\\1.png")
  
  # Full path for the new file
  new_file_path <- file.path(file.path(datapath, "airesults"), new_name)
  
  #print(new_file_path)
  # Rename the file
  file.rename(file_path, new_file_path)
}


cossim = function(a, b) {
  sum(a * b) / (sqrt(sum(a * a)) * sqrt(sum(b * b)))
}


quickrow = function(d){
  x = d #%>%   arrange(if (des) desc(cosine) else (cosine)) %>% 
  # filter(!duplicated(subject_id) ) %>%  # 1 example per person max
  # group_by(trial_number) %>%  slice(1:round((n/3))) %>% 
  #   ungroup() %>% 
  #   arrange(if (des) desc(cosine) else (cosine)) %>% 
  #   head(n)
  x %>% select(id, prompt) %>% print
  x %>% pull(filepath) %>% 
    image_read %>% 
    image_resize("200x") %>% 
    # image_annotate(vals, size = 30, color="red") %>% 
    image_append()
  
}



examplerow = function(d, stims, n = 10, des = TRUE, ref=T) {
  # Get unique trial numbers
  trial_numbers <- d$trial_number %>% unique()
  
  # Initialize an empty magick image object to accumulate all rows
  final_image <- image_blank(width = 1, height = 1) # Placeholder, will be removed later
  
  for (trial in trial_numbers) {
    # Find the stimulus image for this trial
    stim_path <- stims[grep(trial, stims)]
    if (length(stim_path) == 0) {
      message("No stimulus found for trial: ", trial)
      next
    }
    
    # Read and resize the stimulus image
    if(ref){
      stim_image <- image_read(stim_path) %>%
        image_resize("200x") %>% 
        image_border(color = "gray20", geometry = "1x1") %>% 
        { image_append(c(., image_blank(15, 200, "white"))) } %>% 
        image_border(color = "white", geometry = "0x3")
    } else {
      stim_image=image_blank(0,200)
    }
    
    
    
    # Filter data for the current trial number
    x <- d %>%
      filter(trial_number == trial) %>%
      arrange(if (des) desc(cosine) else cosine) %>%
      filter(!duplicated(subject_id)) %>%
      #group_by(trial_number) %>%
      #slice(1:round((n / 3))) %>%
      ungroup() %>%
      arrange(if (des) desc(cosine) else cosine) %>%
      head(n)
    
    # Print example details for inspection
    #x %>% select(id, prompt) %>% print()
    
    # Read and resize images for this trial, then horizontally append them with the stimulus image at the start
    cosine_values <- x %>% pull(cosine) %>% round(2)
    trial_images <- x %>%
      pull(filepath) %>%
      image_read() %>%
      image_resize("200x") %>% 
      image_annotate(., 
                     text = (cosine_values),  # Convert rounded values to text
                     size = 26, 
                     gravity = "northwest",  # Position text in top-left
                     color = "black") %>% 
      image_annotate(., 
                     text = (cosine_values),  # Convert rounded values to text
                     size = 25, 
                     gravity = "northwest",  # Position text in top-left
                     color = "white") %>% 
      image_border(color = "white", geometry = "2x4") 
    
    # Prepend the stimulus image and then append the trial images horizontally
    row_image <- image_append(c(stim_image, trial_images))
    
    # Append the current row to the final image vertically
    final_image <- c(final_image, row_image)
  }
  
  # Remove the initial placeholder image and vertically stack all rows
  final_image <- final_image[-1] %>% image_append(stack = TRUE)
  
  return(final_image)
}


gconvert <- function(image, title_text = "") {
  # Get image dimensions and calculate aspect ratio
  image_info <- image_info(image)
  aspect_ratio <- image_info$height / image_info$width
  
  # Convert the magick image to a raster object
  image_raster <- as.raster(image)
  ggplot() +
    annotation_raster(image_raster, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    coord_fixed(ratio = aspect_ratio) + # Maintains the aspect ratio
    ggtitle(title_text)+
    theme_void()+ # Removes any axes, grid, and background
    theme(plot.margin = margin(1,2,1,2),
          plot.background = element_rect("white", "white"),
          plot.title = element_text(size=9)
    )
}




colorfulness = function(path){
  tmp = image_read(path) %>% image_raster() %>% .$col %>% hex2RGB() 
  # input must be from image_raster(baseline)$col %>% hex2RGB() 
  # implements M3 measure from "Measuring Colorfulness in Natural Images"
  # they show the rgb one is basically as good if not better than the lab/chroma one
  # but grayscale images get 0 though regardless of number of shades of gray.
  tmp = tmp %>% coords()  # rgb
  rg = tmp[,1]-tmp[,2]    # r-g
  yb = 0.5*(tmp[,1]+tmp[,2]) - tmp[,3]
  return(
    sqrt(var(rg)+var(yb))+(0.3*sqrt(mean(rg)^2 + mean(yb)^2))
  )
}


examplepairs = function(var, fd=filedata){
  x=fd %>%  
    {rbind(slice_min(., !!sym(var), n=1, with_ties = FALSE),slice_max(., !!sym(var),n=1, with_ties = FALSE))}
  print(x$prompt)
  x %>%  pull(filepath) %>% 
    image_read() %>%
    image_resize("200x") %>% 
    image_border(color = "white", geometry = "2x2") %>% 
    image_append() %>% gconvert
}

# theme for plots
th= theme_bw()+
  theme(plot.background = element_rect("white", "white"),
        plot.title = element_text(size=9),
        axis.title.x = element_blank(),
        strip.background = element_rect(fill="white"),
        strip.text = element_text(hjust=0),
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        axis.title.y = element_text(size=9),
        plot.margin = margin(b=0)
  )
nopadding = plot_annotation(theme=theme(plot.margin = margin(0,0,0,0)))
