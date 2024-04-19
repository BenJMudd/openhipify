# Auxiliary Functions

We have provided two functions to standardise the the OpenCL procedure for creating devices
and building programs from source. Include **defs.h** in your file containing the function calls,
and include **clbuild.c** in your compilation process.

## Device creation

```c
cl_device_id create_device();
```

Openhipify currently only supports singular device codebases. In order to port a
OpenCL program with multiple device functionality, we have provided a simple function for
creating a single device.

## Program creation

```c
cl_program build_program(cl_context ctx, cl_device_id dev, const char* filename);
```

There are a multitude of ways to load and build kernels in OpenCL, with certain methods
not being supported in Openhipify. One example is to write the kernel directly into the OpenCL
program in a string, and then load this. Openhipify has trouble with translation with methods
similar to this as we do not want to cull non-OpenCL functions. We therefore
include this auxiliary function where kernels are instead located in a separate OpenCL
kernel file (suffix .cl), and then loaded in with this call.
