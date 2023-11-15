export interface IElevatorPersistence {
    _id: string;
    domainId:string;
    floorList: string[];
    code: string;
    buildingId: string;
    salt: string;
  }
  