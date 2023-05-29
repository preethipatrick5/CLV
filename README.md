# Readme
### Steps to run the project
1. Extract the rar file.
2. Open CLV.Rproj file
3. In the console window enter the command `install.packages('pacman)`
4. Install Tensorflow(Please refer install tensorflow section)
5. Open main.R file
6. Run the main.R file

### Known issue
* The session sometimes keeps getting restarted, this is noted primarily on machine with a discreet GPU. The fix is to run the code with SHOULD_PLOT set to FALSE(in line 79 in main.R) once and then turn in back to TRUE again and run again for plots to show up as well.
* It might take two runs before pacman installs and loads every requirement.
### Install Tensorflow
1. In the console window type the command`install.packages('tensorflow')`.
2. Load the Tensorflow library installed with `library(tensorflow)`.
3. Use the `install_tensorflow()` function to install Tensorflow.
### Note
In case a miniconda installation is missing you can do the following steps.
* `install.packages('installr')`
* `library(installr)`
* `install.conda()`