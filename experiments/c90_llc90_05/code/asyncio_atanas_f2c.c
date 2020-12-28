void asyncio_num_cores_set_( int *NumCores ) { 
  extern int numRanksPerNode;

  numRanksPerNode = *NumCores;
}

int asyncio_isionode_(int *rank, int *totalNumNodes, int *numIONodes) {
  return isIORank(*rank, *totalNumNodes, *numIONodes);
}

