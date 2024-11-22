##################### #
# This .R script file contains code to replicate the study
# Author: Andres Karjus, 2024
##################### #

# Install these R packages if missing:
library(ordinal)
library(lme4)
library(tidyverse)
library(RCurl) # for base64; if using openai api.
# Only for plots:
library(patchwork)
library(ggbeeswarm)
library(colorspace)
library(magick)


### Instructions for Python:
#
# If setting up a reticulate Python env, uncomment and run these
# (only needed if replicating image embeddings part or the GPT parts)
library(reticulate)
# conda_create("r-clip", python_version = "3.9", packages = "python=3.9") 
use_condaenv("r-clip")
# conda_install("Pillow", envname = "r-clip")
# conda_install("pydantic", envname = "r-clip")
# conda_install("transformers", envname = "r-clip")
# conda_install("openai", envname = "r-clip")
# conda_install(c("pytorch"), envname = "r-clip") # =1.7.1", "", python_version =  "3.9" ) # transformers backend




#### Variables and paths ####

# Define path where the data and functions script is is:
datapath = "C:/..." 
# expects data is in this folder: trials and demographics csv files in the root and 
# images in subfolders results (all generated images) and stims (stimuli images).


# If planning to replicate the GPT results, need OpenAI key and sufficient credit.
py_run_string(
  '
import os
import openai
import io
from openai import OpenAI
client = OpenAI(api_key="sk-abcdefg") #  <- OpenAI key here
'
)
## ------ End user parameters ---- ##




#### Precomputed data ####

# For partial replication, just load this RData file - it contains all 
# the data objects computed below. It enables recreating all graphs and models.
load(file.path(datapath, "precomputed.RData"))
#
# It contains these R objects:
# save(stimvectors, resvectors, aivectors, copyinputs, creinputs, aicopydata, aicredata,  metadata, demogdata, resdata, aicomp, expdata, filedata, prout, sentout, file=file.path(datapath, "precomputed.RData"))

# The main data object (below as metadata and resdata) containing the prompts,
# participant info, image file names, and cosine similarities
# Is also available as a CSV file cosines.csv in the repo.
# write_csv(resdata, file = file.path(datapath, "cosines.csv"))




# The rest of this script: just run everything for full replication, 
# or selectively for partial (if so, likely need to use the precomputed data above)



#### Load and filter data ####

## Fixed values and objects and loading data:

metafile = file.path(datapath, "trials.csv")
demogfile = file.path(datapath, "demographics.csv")
stims = list.files(file.path(datapath, "stims"), full.names = T )
source(file.path(datapath, "genai_experiment.R")) # load functions

exclude_trials = c(
  "670cff108fc41826ca81e137", # single letter "i" as prompt
  "66fd16098fc41826ca81d7c8", # participant seems to have misunderstood the task, prompts for zuckerberg; recovers after that
  "66ffc5658fc41826ca81da98", # the "castlereate", lead and prompt merged (bug?)
  "66ffc3368fc41826ca81da84"  # participant clearly confused, asks to "add", but recovers after that
  )

# map between columns from the database
column_mapping <- c(
  "Q2_F2_Tree_C" = "FaithfulQ2",
  "Q3_F3_landscape_C" = "FaithfulQ3",
  "Q5_F5_People_C" = "FaithfulQ5",
  "QA1_A1_Car_C" = "Aesthetics Q1",
  "Q42_A2_Cartoon_C" = "Aesthetics Q2",
  "QA3_A3_Penguin_C" = "Aesthetics Q3",
  "Q4_F4_Apples_C" = "FaithfulQ4",
  "Q1_A4_Castle_C" = "FaithfulQ1"
)

# Note: FaithfulQ1 is actually AestheticQ4; fixed below. 

demogdata = read_csv(demogfile) %>% 
  pivot_longer(
    cols = ends_with("_C"),  # Selects all columns ending with '_C'
    names_to = "trial_code",  # New column for the names of the original columns
    values_to = "curation"  # New column for the values
  ) %>%
  select(subject_id, trial_code, curation, Experience, experiment_name, `Duration..in.seconds.`) %>%
  rowwise %>% 
  mutate(trial_number = column_mapping[trial_code]) %>% 
  filter(!grepl("Training", trial_code)) %>% 
  rename(experiment_name1 = experiment_name) # easier to just rename here
any(is.na(demogdata$trial_number))

metadata = read_csv(metafile) %>% 
  rename(number=`...1`) %>% 
  filter(!(trial_id %in% exclude_trials)) %>% # couple excluded trials
  mutate(filepath=file.path(datapath, "results", filename)) %>% 
  mutate(reference = file.path(datapath, "stims", paste0(trial_number, ".png"))) %>% 
  filter(!(trial_number %in% c("Training1", "Training2"))) %>%   # remove training data
  mutate(trial_type = case_when(
    trial_number == "FaithfulQ1"~"Creative",  # important: this is labeled copy in system but is creative
    grepl("Aesthetics", trial_number) ~ "Creative",
    grepl("Faithful", trial_number) ~ "Copying",
    T~"Problem labeling!" # shows up if some mismatch
    )) %>% 
  arrange(desc(created_at)) %>% filter(!duplicated(filename)) %>% # remove first attempts of duplicates
  arrange(number) %>% 
  # add AI experience and image curation data
  left_join(demogdata, by=c("subject_id", "trial_number")) %>%  # assumes duplicate trials removed above
  mutate(imagenumber = str_extract(filename, "_\\d+\\.png") %>%  # Extract "_n.png"
           str_remove_all("_|\\.png") %>%  as.numeric() %>% {.+1} # filenames start from 0
         ) %>% 
  mutate(choice = imagenumber == curation)

# metadata %>% filter(!duplicated(trial_id)) %>% mutate(nc=nchar(prompt)) %>% arrange(nc) %>% select(trial_id, prompt) %>% {rbind(head(.), tail(.))} %>% as.data.frame

# metadata %>% filter(duplicated(filename) | duplicated(filename, fromLast = T)) %>% group_by(filename) %>% mutate(x=paste(unique(subject_id), collapse=",")) %>% select(x, experiment_name) %>% as.data.frame # duplicates gone


metadata %>% count(trial_type)
# 1 Copying     1568
# 2 Creative    1580
metadata %>% count(trial_type, experiment_name)
# 1 Copying    Artist_final      792
# 2 Copying    Laypeople_final   776
# 3 Creative   Artist_final      796
# 4 Creative   Laypeople_final   784
nrow(metadata) # 3148

# Sanity checks
all(stims %in% metadata$reference)
setdiff(  metadata$reference, stims)
setdiff(metadata$filename, list.files(file.path(datapath, "results"))) # all trials entries in files
setdiff(list.files(file.path(datapath, "results")), metadata$filename) %>% length # files not in trials: 1464 - could file.remove those (1476 after removing the few trials)
any(metadata$filename %in% setdiff(list.files(file.path(datapath, "results")), metadata$filename))






#### Get GPT results ####

copyprompt = 'You are an expert at visual art, photography, drawing, painting, art history, and an expert prompter. Your task is to create an image by making use of a generative AI model, our "image machine". To do so, you need to provide text inputs, so-called "prompts". You are being shown an image. Your goal is to write a detailed prompt that would result in an image that is as close as possible to this image in all aspects, including main subjects and where they are on the image, and image composition, colors, style or genre, etc. Describe the shape, size and color of prominent objects, and  relevant characteristics (gender, age, appearance, ethnicity, etc) of any focal human subjects. The prompt should be up to 30 words in length, so be concise. Do not comment, just provide the 30-word prompt without quotes.'


creprompt = c(
  'You are a creative visual artist and an expert prompter. Your task is to create an image by making use of a generative AI model, our "image machine". To do so, you need to provide text inputs, so-called "prompts". You are being shown an image. Your goal is to write a prompt that will make a new image as different from the original as possible. Pro tip: Ideally, you should try to aim for creating an image with different content as well as different visual style, as unrelated to the original as possible. Also, negation does not work very well here - if you state "an image with no elephants", you are actually more likely to get an image with elephants! So do not use negation in the prompt.\nNow look at this image. Try to write a prompt that would create an image that is as different as possible from this image. The prompt should be up to 30 words in length in total but not longer. However, you are constrained in that your prompt MUST start with this phrase describing the original image:\n',
  #"A cartoon landscape with trees"
  '\n\nStart with that phrase, but then try to write the rest of the prompt so that its resulting image would actually not contain or would obscure anything mentioned in that phrase. Use clever wordplay and think outside the box to get away from the meaning and appearance of the reference image and its descriptive phrase. Your prompt, while containing this initial phrase, should describe a new image that would look as different as possible from the reference image you are seeing right now, and would be nothing like ',
  #A cartoon landscape with trees
  '. Do not comment, just provide the 30-word prompt without quotes.'
)


copystims = metadata %>% filter(!duplicated(trial_number), trial_type=="Copying") %>% select(trial_number, trial_type, reference) %>% mutate(prompt=copyprompt) %>% mutate(filepath=reference)  

crestims = metadata %>% filter(!duplicated(trial_number), trial_type=="Creative") %>% select(trial_number, trial_type, reference, prompt) %>% 
  mutate(lead=case_when(
    grepl("^Painting of a penguin", prompt) ~ "Painting of a penguin", 
    grepl("^Photograph of a medieval castle", prompt) ~ "Photograph of a medieval castle", 
    grepl("^A cartoon landscape with trees", prompt) ~ "A cartoon landscape with trees", 
    grepl("^A photo of a gray classic Mini Cooper", prompt) ~ "A photo of a gray classic Mini Cooper", 
  )) %>% 
  mutate(filepath=reference)  %>% 
  select(-prompt) %>% 
  mutate(prompt=paste0(creprompt[1],lead,creprompt[2], lead, creprompt[3]       )) 


copyinputs = 
  get_prompts_openai(test=copystims , gptmodel="gpt-4o-2024-08-06", promptcolumn="prompt", pathcolumn="filepath", temp=0.7, verbose=F, statss=T, visiondetail="high", nvariants=10L, maxwords=30, maxtokens=50L)

creinputs = 
  get_prompts_openai(test=crestims, gptmodel="gpt-4o-2024-08-06", promptcolumn="prompt", pathcolumn="filepath", temp=1.5, verbose=F, statss=T, visiondetail="low", nvariants=10L, maxwords=30, maxtokens=50L) 
# for some reason higher temp and high visiondetail lead to model refusing requests.

creinputs %>% rowwise() %>%  filter(!grepl(lead,imageprompt)) %>% ungroup # all good, prefixes present.


# Get images from generator API based on prompts

py_run_string( # run this
'
from typing import Optional, List, Literal
from pydantic import BaseModel
import uuid
import requests
from io import BytesIO
import numpy as np
#from PIL import Image
from PIL import Image as PilImage
BACKEND_URL = "https://creative-ai-ii-fastapi.chm.mpib-berlin.mpg.de"
GLOBAL_SEED = 42 # this seed was used in the experiment.
class Image(BaseModel):
    url: str

class GeneratedImagesResponse(BaseModel):
    requestId: str
    images: Optional[List[Image]]
    celery_task_id: Optional[str]
    user_id: Optional[str]
    status: Literal["scheduled", "completed"]


class GenerateImagesRequest(BaseModel):
    requestId: str = str(uuid.uuid4())
    async_req: bool = False
    prompt: str
    guidance_scale: Optional[float] = 0
    inference_steps: Optional[int] = 4
    num_images: Optional[int] = 1
    seed: Optional[int] = 42
    filenames: Optional[List[str]] = None

def get_images(prompt: str, num_images=4, seed=GLOBAL_SEED) -> List[str]:
    np.random.seed(seed)
    seeds = np.random.randint(0, 1000, num_images).tolist()

    image_urls = []
    for i in range(num_images):
        request = GenerateImagesRequest(
            prompt=prompt,
            seed = seeds[i],
            )
        response = requests.post(
            f"{BACKEND_URL}/generate/test-user/",
            json=request.model_dump()
        )

        response = GeneratedImagesResponse(**response.json())

        for image in response.images:
            image_urls.append(image.url)

    return image_urls
')


aicopydata = getimages(copyinputs, datapath)
aicredata = getimages(creinputs, datapath)

files <- list.files(file.path(datapath, "airesults"), pattern = "*.png", full.names = TRUE)

# Apply the rename function to each file in the folder
lapply(files, rename_files)




#### Embeddings ####

py_run_string(
"
import os
from PIL import Image
from transformers import AutoProcessor, AutoTokenizer, CLIPModel
os.environ['TRANSFORMERS_BACKEND'] = 'pytorch'
from transformers import CLIPProcessor, CLIPModel
import numpy as np

model = CLIPModel.from_pretrained('openai/clip-vit-base-patch16')
processor = CLIPProcessor.from_pretrained('openai/clip-vit-base-patch16')
")

# calculate reference images clip vectors
py_run_string(
  "
def embed(file_list):
    embeddings = {}
    for filepath in file_list:
        if filepath.endswith('.jpg') or filepath.endswith('.png'):
            print(filepath)
            # Load the image
            image = PilImage.open(filepath)
            # Process the image and get embeddings
            inputs = processor(images=image, return_tensors='pt')
            output = model.get_image_features(**inputs)
            # Use only the filename, not the full path
            filename = os.path.basename(filepath)
            # Store the result in the dictionary
            embeddings[filename] = output.detach().cpu().numpy().tolist()
    return embeddings
"
)


py_run_string("stimvectors = embed(r.stims)")
stimvectors = py$stimvectors %>% as.list() %>% lapply(unlist)  # named list, ready for comparison
respaths = metadata$filepath
py_run_string("resvectors = embed(r.respaths)")
resvectors = py$resvectors  %>% as.list() %>% lapply(unlist) 

aipaths = list.files(file.path(datapath, "airesults"), full.names = T)
py_run_string("aivectors = embed(r.aipaths)")
aivectors = py$aivectors  %>% as.list() %>% lapply(unlist) 
# the transparency warning is from the error images that are not included anyway.


load(file.path(datapath, "resvectors.RData"))
load(file.path(datapath, "gptdata.RData"))





#### Construct comparative results ####

resdata = metadata %>% 
  rowwise() %>% 
  mutate(cosine = 
           cossim(stimvectors[[ basename(reference) ]],
                resvectors[[ basename(filepath) ]])
  ) %>% ungroup() 

# If GPT results available:
aidata = 
  rbind(
  aicopydata %>% select( trial_number, trial_type, reference, newfile,nvariant,imvariant),
  aicredata %>% select( trial_number, trial_type, reference, newfile,nvariant,imvariant)
  ) %>% 
  mutate(experiment_name="GPT-4o", filepath=newfile) %>% 
  rowwise() %>% 
  mutate(cosine = 
           cossim(stimvectors[[ basename(reference) ]],
                  aivectors[[ basename(newfile) ]])
  ) %>% ungroup() %>% 
  mutate(subject_id=paste0(trial_type, "_", nvariant)) %>%  # just so the plotter works
  mutate(aggregate_id = paste(trial_type, trial_number, nvariant, sep="_"))


#### Modeling data ####

meancomp = resdata %>% 
  group_by(subject_id, trial_number) %>% # count() %>% arrange(-n)
  summarize(cosine=mean(cosine), experiment_name=first(experiment_name
), trial_type=first(trial_type), Experience=first(Experience)) %>% 
  ungroup()
nrow(meancomp)

aicomp = resdata %>% 
  group_by(subject_id, trial_number) %>% # count() %>% arrange(-n)
  summarize(cosine=mean(cosine), experiment_name=first(experiment_name
  ), trial_type=first(trial_type)) %>% 
  ungroup() %>% select(-subject_id) %>% 
  rbind(
    aidata %>% group_by(aggregate_id) %>% 
      summarize(
        trial_number=first(trial_number),
        cosine=mean(cosine), 
                experiment_name="GPT-4o",
                trial_type=first(trial_type)) %>% 
      ungroup() %>% 
      select(-aggregate_id)
  ) %>% 
  mutate(experiment_name = case_match(experiment_name, "Laypeople_final"~"Laypeople", "Artist_final"~"Artists",  "GPT-4o"~ "GPT-4o")) %>% 
  mutate(experiment_name=relevel(as.factor(experiment_name), "GPT-4o"))
nrow(aicomp)

ordcomp = resdata %>% 
  filter(containsNSFW==0) %>% # nsfw images were blanked for participants, so they could not curate, so here they are filtered out
  group_by(subject_id, trial_number) %>% 
  mutate(simorder = order(cosine)) %>%  # 1 = distant, 4 = closest
  ungroup() %>% 
  filter(choice) %>% 
  mutate(experiment_name = case_match(experiment_name, "Laypeople_final"~"Laypeople", 
                                      "Artist_final"~"Artists"))

#### Models ####

library(lme4)
library(ordinal)

# Averaged image similarities model, copy task
# using model comparison to get p-values
# Experience is a continuous value (categorical would lose ordering information)
m = lmer(cosine ~ experiment_name + Experience + (1|subject_id) +  (1|trial_number) , data=meancomp %>% filter(trial_type=="Copying") )
m0= lmer(cosine ~ Experience+  (1|subject_id) +  (1|trial_number) , data=meancomp %>% filter(trial_type=="Copying") )
summary(m)
# estimate bootstrap
confint(m, method = "boot", boot.type = "perc", nsim = 1000)
# experiment_nameLaypeople_final -0.04152457 -0.011510296

anova(m0,m) 
#     npar     AIC     BIC logLik deviance Chisq Df Pr(>Chisq)    
# m0    5 -1136.3 -1116.4 573.15  -1146.3                        
# m     6 -1145.3 -1121.5 578.65  -1157.3    11  1  0.0009111 ***



# Averaged image similarities model, creative task
m = lmer(cosine ~ experiment_name + Experience+ (1|subject_id) +  (1|trial_number) , data=meancomp %>% filter(trial_type=="Creative") )
m0= lmer(cosine ~ Experience+   (1|subject_id) +  (1|trial_number) , data=meancomp %>% filter(trial_type=="Creative") )
summary(m)
confint(m, method = "boot", boot.type = "perc", nsim = 1000)
# experiment_nameLaypeople_final  0.001674746 0.039349772

anova(m0,m)  
# npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)  
# m0    5 -1062.0 -1042.1 536.01  -1072.0                       
# m     6 -1064.6 -1040.7 538.29  -1076.6 4.5601  1    0.03272 *




# AI comparison, just fixed effects, and no experience obv
m=lm(cosine~experiment_name, data=aicomp %>% filter(trial_type=="Copying") )
summary(m)
confint(m, method = "boot", boot.type = "perc", nsim = 1000)

m=lm(cosine~experiment_name, data=aicomp %>% filter(trial_type=="Creative") )
summary(m)
confint(m, method = "boot", boot.type = "perc", nsim = 1000)

# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               0.87359    0.01104  79.099  < 2e-16 ***
#   experiment_nameArtists   -0.01932    0.01211  -1.596 0.111285    
# experiment_nameLaypeople -0.04090    0.01213  -3.372 0.000814 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.06985 on 429 degrees of freedom
# Multiple R-squared:  0.03606,	Adjusted R-squared:  0.03157 
# F-statistic: 8.025 on 2 and 429 DF,  p-value: 0.0003789




#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               0.71230    0.01389  51.297  < 2e-16 ***
#   experiment_nameArtists    0.04649    0.01522   3.055  0.00239 ** 
#   experiment_nameLaypeople  0.07099    0.01524   4.659 4.24e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.08782 on 432 degrees of freedom
# Multiple R-squared:  0.05239,	Adjusted R-squared:  0.048 
# F-statistic: 11.94 on 2 and 432 DF,  p-value: 8.958e-06



#### Curation models

## Similarity, when curation is taken into account
# Copy
m = lmer(cosine ~ experiment_name + Experience + (1|subject_id) +  (1|trial_number) , data=ordcomp %>% filter(trial_type=="Copying") )
m0= lmer(cosine ~ Experience+  (1|subject_id) +  (1|trial_number) , data=ordcomp %>% filter(trial_type=="Copying") )
summary(m)
# experiment_nameLaypeople -0.025936   0.008005  -3.240
# estimate bootstrap
confint(m, method = "boot", boot.type = "perc", nsim = 1000)
# experiment_nameLaypeople -0.04207329 -0.0104337611

anova(m0,m) 
#  m     6 -1041.8 -1018.2 526.91  -1053.8 10.15  1   0.001443 **

# Creative
m = lmer(cosine ~ experiment_name + Experience+ (1|subject_id) +  (1|trial_number) , data=ordcomp %>% filter(trial_type=="Creative") )
m0= lmer(cosine ~ Experience+   (1|subject_id) +  (1|trial_number) , data=ordcomp %>% filter(trial_type=="Creative") )
summary(m)
# experiment_nameLaypeople  0.026154   0.011278   2.319
confint(m, method = "boot", boot.type = "perc", nsim = 1000)
# experiment_nameLaypeople  0.003130606 0.048224408

anova(m0,m) 
# m     6 -926.41 -902.80 469.21  -938.41 5.3456  1    0.02077 *


## Ordinal models for direct curation comparison
clmm(as.factor(simorder) ~ experiment_name + Experience + (1 | subject_id) +  (1|trial_number), data = ordcomp %>% filter(trial_type=="Copying") ) %>% summary


clmm(as.factor(simorder) ~ experiment_name + Experience + (1 | subject_id) +  (1|trial_number), data = ordcomp %>% filter(trial_type=="Creative") ) %>% summary
# Estimate Std. Error z value Pr(>|z|)
# experiment_nameLaypeople   0.1809     0.1992   0.908    0.364
# Experience                -0.1449     0.1156  -1.253    0.210

# Also could do experience * group.



#### Exploratory models and additions ####



#### Prompt and feedback analytics ####

pr = 'Analyze this short quoted Text describing an image and output a comma-separated list of elements that are mentioned. Output only these keywords where relevant, as explained and examplified (in brackets) here. These are the only valid output terms:
style = if Text mentions artistic style or period (like cubist, expressionism) 
genre = mentions genre (like landscape, still life, fantasy)
medium = mentions type or medium of image (like photo, painting, cartoon)  
colors = mentions any colors (including phrases like colorful or bright colors)
adjectives = contains any adjectives or similar descriptives (like pleasant, misty, dystopian, destroyed)
objects = mentions one or more tangible subjects or objects (like car, trees, buildings, people)  
setting = mentions a setting or background (like library, sea, jungle, wilderness, night)  
action = if Text mentions some action (like reading, sit, standing).\n
How to respond: only a comma-separated keywords from the above if relevant, but do not describe the actual colors or styles, and do not comment! If no none of the above a present, just say None. Here is the Text to analyze:\n"""'

prin = resdata %>% group_by(trial_id) %>% 
  summarize(prompt=first(prompt), 
            cosine=mean(cosine), 
            trial_type=first(trial_type), 
            experiment_name=first(experiment_name))  %>%
  ungroup %>% 
  mutate(cleanprompt=gsub("(^Painting of a penguin[ ,.!?;:-]*)|(^Photograph of a medieval castle[ ,.!?;:-]*)|(^A cartoon landscape with trees[ ,.!?;:-]*)|(^A photo of a gray classic Mini Cooper[ ,.!?;:-]*|(\r\n)+|(\n)+)", "", prompt) ) %>% 
  mutate(pr = paste0(pr, cleanprompt, '"""'))
nrow(prin)

prout = get_prompts_openai(test= prin, gptmodel="gpt-4o-2024-08-06", promptcolumn="pr", temp=0, verbose=F, statss=T, nvariants=1L, maxwords=NA, maxtokens=50L, imageinput = F)


# Sentiment


pr2 = 'Below is a quoted Feedback from an experiment where people used gen AI to create images. We asked them how they feel about AI. Summarize the Feedback sentiment as one of these categories that best matches its primary sentiment:
Positive = mostly postitive, excited, praising or enthusiastic attitude towards AI.
Critical = a critical, concerned or dissatisfied attitude or talks about flaws or worries or implies AI is not good for art.
Mixed = if Feedback expresses mixed or ambivalent feelings or mentions both positive and negative aspects (eg "fun but boring").
Neutral = pragmatic or indifferent Feedback, or neutral discussion about technical aspects without sentiment or emotion (eg "task was challenging" - this is not negative about AI as such discusses the task they completed beforehand).\n
Output a single category, do not comment! This is the Feedback:\n"""Opinion on generative AI: '


sentin = read_csv(demogfile) %>% select(Feel, experiment_name) %>% 
  mutate(Feel = gsub("(\r\n|\n)", "", Feel)) %>% 
  filter(!is.na(Feel),  !(Feel %in% c("yes", "✌️"))) %>% 
  mutate(pr = paste0(pr2, Feel, '"""'))
nrow(sentin)

sentout = get_prompts_openai(test= sentin , gptmodel="gpt-4o-2024-08-06", promptcolumn="pr", temp=0, verbose=F, statss=T, nvariants=1L, maxwords=NA, maxtokens=50L, imageinput = F)

sentout %>% group_by(experiment_name) %>% count(result)

tags <- c("style", "genre", "medium", "colors", "objects", "setting", "action", "adjectives")

# test
prout %>% 
  mutate(
    values_list = str_split(result, ",\\s*"), # Split values into lists
    invalid_values = map(values_list, ~ setdiff(.x, tags)) # Check for invalid values
  ) %>% 
  filter(map_lgl(invalid_values, ~ length(.x) > 0)) %>% 
  select(cleanprompt, result)
# all ok



prdata = prout %>%  
  group_by(trial_type) %>% 
  mutate(length = scale(nchar(cleanprompt))[,1] ) %>% 
  mutate(cosinescaled = case_when(trial_type=="Copying" ~ scale(cosine)[,1],
                                  trial_type=="Creative" ~ scale(1-cosine)[,1]
  )) %>% 
  ungroup() %>% 
  mutate( values = gsub(", people", "", result) ) %>%
  separate_rows(values, sep = ",[ ]*") %>% 
  mutate(value_present = TRUE) %>% 
  pivot_wider(names_from = values, values_from = value_present, values_fill = FALSE) %>% 
  mutate(across(all_of(c(tags, "None")), ~ replace_na(., FALSE))) %>%
  mutate(across(all_of(c(tags, "None")), ~ ifelse(. == TRUE, "yes", "no"))) %>% 
  mutate(objects = case_when(trial_type=="Creative" ~ "yes", T~objects),
         medium = case_when(trial_type=="Creative" ~ "yes", T~medium) # creative has obj and med in the locked part of the prompt, was removed/not evaled in the text analysis above.
  ) %>% 
  rowwise() %>% 
  mutate(ntags = sum(across(all_of(tags), ~ . == "yes")) ) %>%
  group_by(trial_type) %>% 
  mutate(ntagsscaled = case_when(trial_type=="Copying" ~ scale(ntags)[,1],
                                 trial_type=="Creative" ~ scale(ntags)[,1]
  )) %>% 
  ungroup() %>% 
  select(-action)
# Excluding action, as the results are inconsistent + most of the lower results are all about the sitting.

m=lm(cosinescaled~  style + genre + medium + colors + objects + setting + adjectives + 
       length , data=prdata) %>% summary %>% coefficients 
# %>% coefficients %>% round(2) %>% as.data.frame() %>% select(Estimate, `Std. Error`, `Pr(>|t|)`) %>% xtable::xtable()
regression_data <- as.data.frame(m)
regression_data$Predictor <- rownames(m)
regression_data <- regression_data %>%
  rename(
    Estimate = `Estimate`,
    StdError = `Std. Error`,
    PValue = `Pr(>|t|)`
  ) %>%
  mutate(
    CI_Lower = Estimate - 1.96 * StdError,
    CI_Upper = Estimate + 1.96 * StdError
  ) %>% 
  mutate(dotcolor = case_when(PValue >= 0.05 ~ "a", Estimate<0~"b", T~"c" )) %>% 
  mutate(Predictor=gsub("yes","", Predictor)) %>% 
  filter(Predictor !="(Intercept)")
regression_data$Predictor <- factor(regression_data$Predictor, levels = regression_data$Predictor %>% rev )
# This object is used for plotting below.





#### Plots ####

# The ggsave() puts the plots in the default working directory, see getwd()

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


### Example image grids

(
  (resdata %>% filter(trial_type == "Copying") %>% examplerow(stims, n = 4, des = TRUE) %>% 
     gconvert("(A) Copy task, make it similar: top most similar") ) +
    (resdata %>% filter(trial_type == "Copying") %>% examplerow(stims, n = 4, des = FALSE, ref=F) %>% 
       gconvert("(B) Copy task: bottom least similar ") ) 
)/
  (
    (resdata %>% filter(trial_type == "Creative") %>% examplerow(stims, n = 4, des = FALSE) %>% 
       gconvert("(C) Creative task, make it different: top most different ones") )+
      (resdata %>% filter(trial_type == "Creative") %>% examplerow(stims, n = 4, des = TRUE, ref=F) %>% gconvert("(D) Creative task: the bottom, image remains similar") ) 
  ) + plot_annotation(theme=theme(plot.margin = margin(0,0,0,0)))

ggsave("figure_examplegrid_full.png", width=2000, height=1900, units="px")


(
  resdata %>% filter(trial_type == "Copying") %>% examplerow(stims, n = 4, des = TRUE) %>% 
    gconvert("(A) Copy task, make it similar: top closest copies") ) +
  
  (
    (resdata %>% filter(trial_type == "Creative") %>% examplerow(stims, n = 4, des = FALSE) %>% 
       gconvert("(B) Creative task, make it different: top creative results") )
  ) + plot_annotation(theme=theme(plot.margin = margin(0,0,0,0)))

ggsave("figure_examplegrid_best.png", width=2000, height=850, units="px")


(
  (aidata %>% filter(trial_type == "Copying") %>% examplerow(stims, n = 4, des = TRUE) %>% 
     gconvert("(A) Copy task, make it similar: top most similar") ) +
    (aidata %>% filter(trial_type == "Copying") %>% examplerow(stims, n = 4, des = FALSE, ref=F) %>% 
       gconvert("(B) Copy task: bottom least similar ") ) 
)/
  (
    (aidata %>% filter(trial_type == "Creative") %>% examplerow(stims, n = 4, des = FALSE) %>% 
       gconvert("(C) Creative task, make it different: top most different ones") )+
      (aidata %>% filter(trial_type == "Creative") %>% examplerow(stims, n = 4, des = TRUE, ref=F) %>% gconvert("(D) Creative task: the bottom more similar ones") ) 
  ) + plot_annotation(theme=theme(plot.margin = margin(0,0,0,0)))

ggsave("figure_examplegrid_gpt.png", width=2000, height=1900, units="px")



x = resdata %>% filter(subject_id=="FS_85ZLVMer9PrJMic") 
(
  x %>% filter(trial_type == "Copying") %>%
    mutate(subject_id=runif(n())) %>% # hack, get all of one subject
    examplerow(stims, n = 4, des = T) %>% 
    gconvert("(A) One participant's generations in Copy task") 
)+
  (
    x %>% filter(trial_type == "Creative") %>%
      mutate(subject_id=runif(n())) %>% # hack, get all of one subject
      examplerow(stims, n = 4, des = F) %>% 
      gconvert("(B) The participant's generations in Creative task") 
  ) + plot_annotation(theme=theme(plot.margin = margin(0,0,0,0)))
ggsave("figure_example_one.png", width=2000, height=880, units="px")

x$prompt %>% unique %>% paste(collapse="\n") %>% cat

metadata %>% group_by(experiment_name) %>% summarize(n=n_distinct(subject_id))





### Summary plots ###
# Experience; duration, filesize, promptlength


expdata = read_csv(demogfile) %>% 
  filter(subject_id %in% metadata$subject_id) %>% # one excluded later
  filter(!duplicated(subject_id)) %>% 
  select(experiment_name, subject_id, Experience, `Duration..in.seconds.`) %>% 
  mutate(dur =  `Duration..in.seconds.`/60)

filedata = metadata %>% 
  mutate(
    promptlength = nchar(prompt),
    filesize = file.info(filepath)$size / 1024 ) %>%  # KB
  rowwise %>% 
  mutate(colorfulness = colorfulness(filepath)) %>% # slow
  ungroup

summary(lm(filesize~experiment_name, data=filedata))



g=ggplot(filedata %>% 
         group_by(trial_id) %>% 
         mutate(promptlength = ifelse(row_number() == 1, promptlength, NA_integer_)) %>%
         ungroup %>% 
         select(experiment_name, filesize, colorfulness, promptlength) %>% 
         pivot_longer(
           cols = -experiment_name, 
           names_to = "variable",  
           values_to = "value") %>% 
         mutate(experiment_name = case_match(experiment_name, "Laypeople_final"~"Laypeople", "Artist_final"~"Artists"),
                variable = case_match(variable,
                                      "colorfulness"~"Colorfulness",
                                      "filesize" ~ "File size (KB)",
                                      "promptlength" ~ "Prompt length (chr)"
                )
                ) 
       , 
       aes( value, experiment_name, color=experiment_name))+
  facet_wrap(~variable, nrow=1, scales = "free_x")+
  geom_beeswarm(size=0.04,cex=0.9, priority = "random")+
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 16,          
    size = 0.6,        
    color = "black"
  ) +
  stat_summary(
    fun.data = mean_cl_normal,  # Provides mean and CI
    geom = "errorbar",
    width = 0.1,                # Controls the T-bar width
    color = "black",
    size=0.2
  ) +
  scale_color_manual(values = c( "#999999", "#cc7722") %>% rev, guide="none")+
  labs(title="(A) Image level summary data")+
  th+
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(angle=90, hjust=0.5),
        panel.spacing = unit(0.5, "lines"))


g2=
  ggplot(expdata %>% 
         mutate(#Experience=as.factor(Experience),
           experiment_name = case_match(experiment_name, "Laypeople_final"~"Laypeople", "Artist_final"~"Artists")  %>% 
            as.factor %>% relevel(.,"Laypeople"),
           Exp = as.factor(Experience)
           ) ,
       aes( x = experiment_name, fill=Exp, color=experiment_name )) +
  geom_bar( stat = "count", position = position_dodge(),width = 0.7, linewidth=0.5) +
  scale_fill_manual(values = c( darken( lighten("#999999",0.4), c(0, 0.2, 0.4, 0.6))), labels=c("none", "basic", "interm.", "expert"))+
    scale_x_discrete(expand=c(0.2,0.2))+
  scale_color_manual(values = c( "transparent", "#cc7722"), guide="none")+
  scale_y_continuous(expand=c(0.01,0))+
  labs(title="(D) Participants' AI experience level", fill="", y="Count")+
  #annotate("text", x=2, y=c(1,49), hjust=0.5, vjust=c(0,1), size=2.5, color="white", label=c("none", "basic", "intermediate", "expert") %>% rev)+
  th+
  theme(legend.title = element_text(size=7),
       axis.title.y = element_text(size=7,margin = margin(0,0,0,0)),
       legend.key.size = unit(0.3, "cm"),  # Adjusts the size of the legend keys
       legend.text = element_text(size = 7, margin=margin(0,0,0,0)),
       legend.spacing.x = unit(0, "cm"),
       legend.position="right",
       plot.margin=margin(t=-10),
       legend.margin = margin(l= -5)
        )


g11 = filedata %>% left_join(resdata %>% select(filepath, cosine, trial_type), by="filepath") %>% filter(trial_type=="Copying") %>% 
  ggplot(aes(cosine, promptlength, color=experiment_name))+
  geom_point(size=0.4)+
  geom_smooth(method="lm", color="black")+
  th+
  theme(axis.title.x = element_text(size=9), legend.position="none")+
  labs(x="Similarity", y="Prompt length", title="(B) Copy task prompts and success")+
  scale_color_manual(values = c( "#999999", "#cc7722") %>% rev, guide="none")

e1=examplepairs(var="colorfulness")+labs(title="(C) Min/max examples")+theme(plot.title=element_text(size=9))
e2=examplepairs(var="filesize")
e3=examplepairs(var="promptlength")

l=
"AAABB
AAABB
AAABB
CDEFF
CDEFF"
ggsave( ( g+g11 +
         e1+e2+e3+wrap_elements(g2)
        )+
         plot_layout(design=l)+
          plot_annotation(theme=theme(plot.margin = margin(0,0,0,0))),
        filename= "figure_summaries.png", width=2000, height=1100, units="px")


# Caption experience sentence:
expdata %>% group_by(experiment_name) %>% summarize(mean(Experience-1))
                                 
                                 




### Results plots ###


g=ggplot(aicomp %>% 
         mutate(experiment_name=relevel(experiment_name, "Artists"),
                experiment_name=relevel(experiment_name, "Laypeople"),
                trial_type = case_match(trial_type, "Creative" ~ "Creative - make it different", "Copying" ~ "Copying - make it similar")
         )
         , aes(experiment_name, cosine, color=experiment_name))+
  geom_beeswarm(priority = "random", size=0.5, cex=1.5)+
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 16,           # Dash shape for the mean
    size = 1.3,        
    color = "black"
  ) +
  stat_summary(
    fun.data = mean_cl_normal,  # Provides mean and CI
    geom = "errorbar",
    width = 0.1,                # Controls the T-bar width
    color = "black",
    size=0.4
  ) +
  scale_color_manual(values = c( "#999999", "#cc7722",  "#377eb8"), guide="none")+
  facet_wrap(~trial_type, nrow = 1)+
  theme_bw()+
  th+
  scale_y_continuous(limits = c(0.4,1), expand = c(0,0))+
  labs(title="(A) Averaged results, no curation", y="Similarity to reference image")

g2 = ggplot(ordcomp %>% mutate(experiment_name=relevel(as.factor(experiment_name), "Laypeople"))
              , aes(experiment_name, cosine, color=experiment_name))+
  geom_beeswarm(priority = "random", size=0.5, cex=1.5)+
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 16,           # Dash shape for the mean
    size = 1.3,             
    color = "black"
  ) +
  stat_summary(
    fun.data = mean_cl_normal,  # Provides mean and 95% CI
    geom = "errorbar",
    width = 0.1,                # Controls the T-bar width
    color = "black",
    size=0.4
  ) +
  scale_color_manual(values = c( "#999999", "#cc7722",  "#377eb8"), guide="none")+
  
  facet_wrap(~trial_type, nrow = 1)+
  theme_bw()+
  th+
  theme(axis.title.x=element_blank())+
  scale_y_continuous(limits = c(0.4,1), expand = c(0,0))+
  labs(title="(B) Participant-curated results", y="")+
  th

  
ggsave(g+g2+plot_layout(widths = c(3,2))+plot_annotation(theme=theme(plot.margin = margin(0,0,0,0))),filename= "figure_comparisons.png", width=2000, height=800, units="px")
  
  
  

# resdata %>% filter(trial_type=="Copying") %>%  group_by(experiment_name) %>% slice_max(cosine, n=5) %>% select(subject_id,cosine,prompt, filepath) %>% as.data.frame()

# resdata %>% filter(trial_type=="Creative") %>%  group_by(experiment_name) %>% slice_min(cosine) %>% select(subject_id,prompt, filepath) %>% as.data.frame()
  


## Prompt analytics plot

g1=ggplot(regression_data, aes(x = Estimate, y = Predictor, color = dotcolor)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray20") +
  geom_point(size = 1) +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.15) +
  geom_text(aes(x=-Inf, label=Predictor), hjust=-0.02, vjust=0.5, size=3, color="gray10")+
  scale_x_continuous(limits = c(-1.2,1.72))+
  scale_color_manual(values = c("gray",   desaturate("#ff7f0e", 0.3) )) +
  labs(
    title = "(A) Regression predicting success",
    y = ""
  ) +
  th+
  theme(legend.position = "none", 
        plot.margin = margin(r=13, b=0, l=0),
        # plot.title.position = "plot", plot.title = element_text(hjust = 0.1),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()
  )


g2=ggplot(
  prdata  %>% pivot_longer(
    cols = c("objects", "setting", "style", "medium", "colors", "adjectives", "genre"), 
    names_to = "Variable",   
    values_to = "Value" ) %>%
    mutate(Value = ifelse(Value == "yes", 1, 0) %>% as.numeric) %>% 
    group_by(experiment_name,Variable) %>% summarize(s=sum(Value)) %>%
    group_by(experiment_name) %>% 
    mutate(s=s/sum(s)*100) %>% 
    mutate(experiment_name = case_match(experiment_name, "Laypeople_final"~"Laypeople", "Artist_final"~"Artists")) %>% 
    ungroup() %>% 
    mutate(Variable=fct_relevel(as.factor(Variable), regression_data$Predictor %>% levels %>% rev)) %>% 
    mutate(experiment_name=experiment_name%>% as.factor %>% relevel("Laypeople"))
  
  , aes(Variable , s, fill=Variable, label=Variable))+
  geom_col(position = position_dodge())+
  geom_text(aes(y=0), angle=90, vjust=0.4, hjust=-0.02, color="white", size=3 )+
  scale_fill_manual(values= palette <- 
                      c("#377eb8", 
                        "#999999", 
                        "#88ccee", 
                        "#e68a00",
                        "#44aa99", 
                        "#cc7722" %>% darken, 
                        "#117733")
  )+
  scale_y_continuous(expand=c(0,0))+
  labs(y="% of prompts with component", title="(B) Relative prompt component frequency")+
  th+
  theme(axis.text.x = element_blank(),
        legend.position = "none",
        panel.spacing = unit(0.6, "lines"), 
        plot.margin=margin(b=0),
        plot.title.position = "plot",
        plot.title = element_text(hjust = 0.1)
  )+
  facet_wrap(~experiment_name)

g3=ggplot(  sentout %>% mutate(experiment_name = case_match(experiment_name, "Laypeople_final"~"Laypeople", "Artist_final"~"Artists")) %>% 
              group_by(experiment_name, result) %>% 
              count %>% 
              ungroup() %>% 
              mutate(experiment_name=experiment_name%>% as.factor %>% relevel("Laypeople")) , 
            aes(experiment_name,n, fill=result))+
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    aes(label = result), 
    position = position_stack(vjust = 0.5), 
    color = "white", size=3
  ) +
  scale_y_continuous(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  th+
  theme(legend.position = "none", 
        axis.text.x = element_blank(), 
        plot.margin=margin(l=10, b=0),
        plot.title.position = "plot",
        plot.title = element_text(hjust = 0.4)
  )+
  labs(y="Count", title="(C) AI sentiment")+
  facet_wrap(~experiment_name, scale="free_x")+
  scale_fill_manual(values = c(
    "Positive" =  "#66c2a5",
    "Critical" = "#e85c00",
    "Mixed" = "#377eb8",
    "Neutral" = "#999999"
  ))

ggsave(g1+g2+g3+plot_layout(widths = c(3,4,2))+nopadding, filename= "figure_promptnlp.png", width=2000, height=800, units="px")





################################ #
