* Current
  - JMC/CH/AT. Adding some export variables for passing things to plot in parent levels.
  - JMC/CH. Creating reference "*hello_world*" experiment that can be used to verify basic data transfer functionality
     is OK and repeatable as code evolves. (i.e. ability to get fields to MITgcm in correct layout from parent layers, 
     ability to get fields back to parent layer with correct layout/values)
  - DM/Matt. Creating foundation for set up that can build and run on basic Linux machine (Gfortran 5.4, Heracles etc...)
  - JMC/CH. Upgrade to recent MITgcm.
  - JMC/CH/AT. Creating/fusing needed code and experiment versioning/tracking repositories for easy checkout/checkin, push/pull, merge/deploy.
  - JMC/CH/AT/DM. Get basic C32 - C48 "*tutorial*/*reference*/*coding*" experiment GEOS-5 coupled system running at reasonable 
     technical level (repeatable, regression tests, various decomp, workstation/pleiades/discover etc...), but not overly tuned.


* Over next 3 months
   - DM/AM/UDI test and help improve code and experiment versioning/tracking setup.
   - DM/AM create first generation llc270 <-> C96(?) coupled set up, with decent climate (ice, tropical ocean etc...)



* Experiment suggestions
  - UDI/AM/PH run llc90 (ECCOv4) with MERRA-[12] atmospheric state bc's and optimize (hand, GF, adjoint)
     uncertain ocean boundary layer scheme parameters, initial conditions, parameterization coeffs
     etc.. to keep trajectory close to ECCOv4 solution. 
  - CNH getting forcing from GMAO for hourly fields.


* Year 1
  - llc270 <-> MERRA-2 state coupled and somewhat tuned (via hand, GF, adjoint combinations as appropriate).
  - llc270 <-> C96(?) [including replay] coupled system, using GEOS-5 CICE components for ice thermodynamics and
      dynamics(?). 
