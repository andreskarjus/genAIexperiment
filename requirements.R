# Install these R packages if missing:
install.packages("ordinal")
install.packages("lme4")
install.packages("tidyverse")
install.packages("RCurl") # for base64; if using openai api.
# Only for plots:
install.packages("patchwork") 
install.packages("ggbeeswarm")
install.packages("colorspace")
install.packages("magick")


### Instructions for Python:
#
# If setting up a reticulate Python env, run these too
# (only needed if replicating image embeddings part or the GPT parts)
install.packages("reticulate")
library(reticulate)
conda_create("r-clip", python_version = "3.9", packages = "python=3.9") 
use_condaenv("r-clip")
conda_install("Pillow", envname = "r-clip")
conda_install("pydantic", envname = "r-clip")
conda_install("transformers", envname = "r-clip")
conda_install("openai", envname = "r-clip")
conda_install(c("pytorch"), envname = "r-clip") # =1.7.1", "", python_version =  "3.9" ) # transformers backend