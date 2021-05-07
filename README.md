# INF-3910-5 2021 Exam 2021

## Introduction

In this exercise you will create a program system for modeling the trajectories
and fate of objects or substances drifting in the ocean (e.g copepods, salmon
lice, garbage...). You will also write a small web application to examine and
visualize results from simulations.

In order to perform real simulations you will be provided with the following
data (in the `data/` folder):

* A grid definition file: `grid.grd`. The grid used in this exercise is
  unstructured and triangular (obtained from the FVCOM ocean model). The grid
  file has three parts:
  * Line 1 is header with two integers: number of elements (triangles) and number of nodes.
  * Lines 2 - (nElements + 1) has three integer, indexing coordinates of the corners of each element (triangle).
  * Lines (nElements + 2) ->  has two real number which are the coordinates of a node in meters in the UTM-33 projection.
* 24 velocity files `uv-N.dat` containing the current velocity components in m/s
  for each node. Each file represents one hour.
* A particle file `particles.dat` with initial positions for particles, in the UTM-33 projection.

### Simulating trajectories

The steps required to simulate the trajectory (movement over time) of a particle are as follows:

1. Read and initialize the grid.
2. Read the velocities (u,v) for the current hour.
3. Locate the element (triangle) which contains the particle coordinates (x, y).
4. Look up the velocity of the element.
5. Advect (move) the particle using the following formula:
   (x', y') = (x + u * dt, y + v * dt), where dt is the timestep (e.g. 60.0-600.0 s)
6. Increment time by dt. If time is larger than the defined simulation time, stop. Otherwise: `goto 2`.

### On coordinates and projections

We well concern ourselves with two coordinates systems, with corresponding
projections: UTM-33 and WGS84. WGS84 is based on spherical coordinates (degrees
latitude and longitude), whereas UTM is Cartesian (meters).

For simulating trajectories it's vastly more convenient to work with
Cartesian coordinates in UTM, than with spherical coordinates.

For plotting features on a map we must use WGS84, as the Leaflet mapping library
only supports WGS84 out of the box.

Getting projections and transformations right is tricky business, so you have
been given two functions `toLatLon` and `fromLatLon` which will do the job for
you. They are available in `src/Server/UTM.fs`.

## Exercises

1. Read and plot particles
   1. Server side
      * Read the particles from `particles.dat`.
      * Create a REST endpoint to fetch the particles as JSON.
   2. Client side
      * Fetch the initial particles from the server
      * Plot the particles on the map.
2. Read and initialize the grid
   1. Server side
      * Read the grid from `grid.grd`.
      * Calculate and print the bounding box (min and max coordinates) of the grid.
      * Write a function to find which element a coordinate is inside (Hint: *triangle interior*).
      * Calculate print the initial element of each particle
      * Create a REST endpoint to fetch the grid.
   2. Client side
      * Fetch the grid from the server and plot it on the map. **NB!** The grid
        is quite big (~50 000 elements), so this will torture your browser.
      * Add a button to toggle the grid visibility.
3. Advect particles
   1. Server side
      * Implement advection for the particles (see above). If a particles ends
        up outside the grid, leave it where it is and ignore it.
      * Implement time stepping for up to 24 hours, using the correct velocity data.
      * Keep the particle positions (simulation frame) for every hour, i.e. 24 frames per simulated day.
      * Implement long simulations by wrapping velocity data.
      * Implement a REST endpoint to fetch frames
   2. Client side
      * Load particle frames from the server and plot the particles
      * Add buttons to time step back and forth through frames
      * Add a play button to animate frames
4. Interactive simulations
   1. Server side
      * Add a REST endpoint to send in initial particle positions, a timestep and simulation length
      * Add an endpoint to run the current simulation
      * Add an endpoint to step the current simulation by one hour
      * Make advection more efficient by restricting the element search space (Hint: *Maps and Sets are your friends*)
   2. Client side
      * Add inputs for defining a new simulation: timestep, simulation time, number of particles, radius
      * Initialize the particles by double-clicking on the map. This should
        randomly add the specified number of particles within the given radius
      * Add buttons to run the whole calculation, or step trough it interactively

## Documentation

A *short* written report must be included. The purpose is to help the
review process. It should list what has been done and what has not, what is
working and what is not. It should also mention anything else of importance,
like if the code is not compiling, design choices, etc.

You will not be graded for the report. But not delivering are report or
misreporting can cause a deduction of points. And remember, keep it short!

### Commenting code

Please comment your code (except when obvious or trivial) to help reviewers
understand your thinking. When implementing algorithms, provide a *short*
description of the strategy and implementation.

## Rules for the exam

The exercise is provided as a home exam, due **7.6.2021 at 12:00**.

1. Use the starter code available in GitHub classroom
2. Do not modify files or code blocks marked *DO NOT MODIFY*.
3. Questions must be asked in the open on Canvas.
4. The code should compile and run using `dotnet run` from the top-level
   directory.
5. The exam must be submitted on WiseFlow as a pdf document with the code
   archive as an attachment:
   1. The submitted code should be cleaned from the following directories:
   `node_modules .fake .fable deploy src/*/bin src/*/obj src/*/js`
   2. The archive should contain the `.git` folder.
   3. The archive should unpack directly into a folder named with the candidate's
      identifier (**not** your name!), e.g. `mv inf-3910-drifter 123; tar fcz 123.tgz 123`
6. If you find a bug in the starter code, please let me know, and I'll post a fix.
7. Any updates to the starter code or to the exercise will be posted to the
   starter repository on GitHub and announced on Canvas.

## Grading

The grading of the exam will be based on the following criteria:

* Functionality, i.e. what works and what does not.
* Usage of functional constructs and functional style.
* Cleanliness of the code and project.

## Starter code

The provided starter code contains a skeleton client-server project. The
project should be compiled and run using `dotnet run` from the top-level folder.
Do not modify files marked as so.

## Install pre-requisites

You'll need to install the following pre-requisites in order to build SAFE applications

* [.NET Core SDK](https://www.microsoft.com/net/download) 5.0 or higher
* [Node LTS](https://nodejs.org/en/download/)

## Starting the application

Before you run the project **for the first time only** you must install dotnet "local tools" with this command:

```bash
dotnet tool restore
```

To concurrently run the server and the client components in watch mode use the following command:

```bash
dotnet run
```

Then open `http://localhost:8080` in your browser.

The build project in root directory contains a couple of different build targets. You can specify them after `--` (target name is case-insensitive).

## Hints

You will find more documentation about the used F# components at the following places:

* [Saturn](https://saturnframework.org/)
* [Fable](https://fable.io/docs/)
* [Feliz](https://zaid-ajaj.github.io/Feliz/)
* [Elmish](https://elmish.github.io/elmish/)
* [Fable.ReactLeaflet](https://github.com/MangelMaxime/Fable.ReactLeaflet/)

Oceanography and particle simulations:

* [FVCOM](http://fvcom.smast.umassd.edu/fvcom/)
* [OpenDrift](https://opendrift.github.io/)

*Good luck, and may the Foo be with you!*
