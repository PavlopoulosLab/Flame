# Flame 2.0 Docker image instructions

### Main instructions
1. Copy the "Flame2.0" directory to the current position

(or edit "Dockerfile" and replace "./Flame2.0" with the path to the Flame2.0 directory  in **line 19**)

2. Make sure that the paths to *Dockerfile*, *Rprofile.site*, and *install_packages.R* are the same (current position)

3. To create the image, run the following:

`docker build --rm -t pavlopouloslab/flame .`

4. After the image has been created, to run, use the following:

`docker run --rm -ti -p 8080:3838 pavlopouloslab/flame`

The above will create a container listening to port **8080**. Visit `http://localhost:8080` to open Flame.



### Other useful tips

1. View all docker images with detailed information
`docker images`

2. View all running containers with detailed information
`docker ps -a --no-trunc`

3. Export an image to a tar archive (to move to another machine)
`docker save -o flame.tar pavlopouloslab/flame`

4. Load an exported docker image
`docker load -i flame.tar`

5. Delete an image by name (e.g. pavlopouloslab/flame)
`docker rmi pavlopouloslab/flame`

6. Delete an image by id (e.g. Flame with image id da0792ab1d1b)
`docker rmi da0792ab1d1b`

7. Delete all obsolete containers (will not delete images, just containers)
`docker system prune -f`

8. Monitor CPU & Memory Usage for all running containers
`docker stats --no-trunc -a`
