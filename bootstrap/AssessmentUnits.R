library(icesTAF)

# Download data from Dropbox
download("https://www.dropbox.com/s/xzktj5nejp6tyn8/AssessmentUnits.zip?dl=1")

# Unzip and delete zip file
unzip("AssessmentUnits.zip")
unlink("AssessmentUnits.zip")

# NB!
# Currently data are maintained in a GitHub repository as WKT by
# Kate Collingridge from CEFAS on behalf of the OSPAR TG-COMP group.
# The latest version is manual compressed into a zip file and placed in dropbox.

# Data could be downloaded directly from the GitHub repository.