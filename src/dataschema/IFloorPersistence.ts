export interface IFloorPersistence {
    _id: string;
    buildingId:string;
    width: number; 
    length: number;
    floorNumber: number;
    description: string;
    salt: string;
  }