export interface CreateElevatorDTO {
  code: string;
  floorList:string[];
  buildingId:string;
}

export interface ElevatorResponseDTO {
  id:string;
  code: string;
  floorList:string[];
  buildingId:string;
}


