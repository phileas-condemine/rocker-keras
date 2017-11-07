First install Docker Toolbox <br>
Create a new folder ~/Documents/keras_project<br>
Then run rocker/rstudio from docker hub<br>
docker run -d -v ~/Documents/keras_project:/home/rstudio/keras_project -p 8787:8787 —name keras_demo rocker/rstudio<br>
then<br>
docker exec -it keras_demo apt-get update<br>
docker exec -it keras_demo apt-get install libxml2-dev<br>
docker exec -it keras_demo apt-get install zlib1g-dev<br>
docker exec -it keras_demo apt-get install python-pip python-dev<br> build-essential<br>

If you want to use tensorboard in the container rather than on your local machine :
docker exec -it keras_demo pip install --upgrade pip<br>
docker exec -it keras_demo pip install --upgrade virtualenv<br>
docker exec -it keras_demo pip install tensorboard<br>
... (tensorboard in container still requires so work TBC)<br>

then log to your rstudio container from the browser<br>
check your docker ip with "docker-machine ip"<br>
let's assume your IP is 192.168.99.100<br>
then in the url bar, type 192.168.99.100:8787 and log in with<br>
user rstudio<br>
password rstudio<br>
then run the first chunk of keras_demo.Rpres to install packages<br>
