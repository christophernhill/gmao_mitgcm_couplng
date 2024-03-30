void asyncio_num_cores_set_( int *NumCores ) { 
  extern int numRanksPerNode;

  numRanksPerNode = *NumCores;
}

void asyncio_ncrn_set_( int *ncrn ) { 
  extern int NumCoresRootNode;

  NumCoresRootNode = *ncrn;
}

int asyncio_isionode_(int *rank, int *totalNumNodes, int *numIONodes) {
  int isIORank(int, int, int); /* prototype */

  return isIORank(*rank, *totalNumNodes, *numIONodes);
}

