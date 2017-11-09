First install Docker Toolbox <br>
Create a new folder ~/Documents/keras_project<br>
Then run rocker/rstudio from docker hub<br>
`docker run -d -v ~/Documents/keras_project:/home/rstudio/keras_project -p 8787:8787 --name keras_demo rocker/rstudio`<br>
Then in the same terminal, execute commands in the container with these lines <br>
`docker exec -it keras_demo apt-get  update`<br>
`docker exec -it keras_demo apt-get  install -y libxml2-dev`<br>
`docker exec -it keras_demo apt-get  install -y zlib1g-dev`<br>
`docker exec -it keras_demo apt-get install -y python-pip python-dev build-essential`<br>
`docker exec -it keras_demo pip install --upgrade pip`<br>
`docker exec -it keras_demo pip install --upgrade virtualenv`<br>

Then log to your rstudio container from the browser<br>
Check your docker ip with `docker-machine ip`<br>
Let's assume your IP is 192.168.99.100<br>
Then in the url bar, type http://192.168.99.100:8787 and log in with<br>
<b>user</b>: rstudio<br>
<b>password</b> : rstudio<br>
Then browse to rstudio/keras_project, open keras_demo.Rpres<br>
If keras_demo.Rpres is not there, in your local computer, put it in ~/Documents/keras_project, now you should see it in the rstudio container<br>
Run the first chunk of keras_demo.Rpres to install keras<br>
Run the second chunk to install other packages<br>
