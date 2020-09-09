library(icesTAF)

# Download data from Dropbox 
download("https://www.dropbox.com/s/tvt1j8afh1afdp5/StationSamples.zip?dl=1")

# Unzip and delete zip file
unzip("StationSamples.zip")
unlink("StationSamples.zip")

# NB!
# Currently data are manual extracted using an SQL script within the assessment
# database and exported and compressed into a zip file placed in dropbox.
# The script query the ICES oceanographic database and extract and classify data
# based on the assessment units boundaries. The assessment units are imported
# into the assessment database beforehand.

# Data could be downloaded through the ICES oceanographic data portal instead
# either using the GUI or the web service behind and then be classified into
# assessment units in the R script. Currently the web service require and email
# address which complicate an fully automatic procedure.