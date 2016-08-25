* Current
  - JMC/CH/AT. Adding some export variables for passing things to plot in parent levels.
  - JMC/CH. Creating reference experiment that can be used to verify basic data transfer functionality
     is OK and repeatable as code evolves. (i.e. ability to get fields to MITgcm in correct layout from parent layers, 
     ability to get fields back to parent layer with correct layout/values)
  - DM/Matt. Creating foundation for set up that can build and run on basic Linux machine (Gfortran 5.4, Heracles etc...)
  - JMC/CH. Upgrade to recent MITgcm.
  - JMC/CH/AT. Creating/fusing needed code and experiment repositories for easy checkout/checkin, push/pull, merge.
  - JMC/CH/AT/DM. Get basic C32 - C48 coupled system running at reasonable technical level, but not
     overly tuned.


* Over next 3 months


* Experiment ideas
  - run llc90 (ECCOv4) with MERRA-[12] atmospheric state bc's and optimize (hand, GF, adjoint)
     uncertain ocean boundary layer scheme parameters, initial conditions, parameterization coeffs
     etc.. to keep trajectory close to ECCOv4 solution. 


* Year 1
