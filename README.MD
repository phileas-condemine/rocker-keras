First install Docker Toolbox
Then run rocker/rstudio from docker hub
docker run -d -v ~/Documents/training\ hackathon\ 2017/keras_demo:/home/rstudio/keras_demo -p 8787:8787 —name keras_demo rocker/rstudio 
then
docker exec -it keras_demo apt-get update
docker exec -it keras_demo apt-get install libxml2-dev
docker exec -it keras_demo apt-get install zlib1g-dev
docker exec -it keras_demo apt-get install python-pip python-dev build-essential 
docker exec -it keras_demo pip install --upgrade pip 
docker exec -it keras_demo pip install --upgrade virtualenv 
docker exec -it keras_demo pip install tensorboard 

then log to your rstudio container from the browser
check your docker ip with "docker-machine ip"
let's assume your IP is 192.168.99.100
then in the url bar, type 192.168.99.100:8787 and log in with
user rstudio
password rstudio
then run the first chunk of keras_demo.Rpres to install packages
