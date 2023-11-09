export interface IElevatorPersistence {
    _id: string;
    domainId:string;
    floorId: string;
    code: string;
    coordX1: number;
    coordY1: number;
    coordX2: number;
    coordY2: number;
    salt: string;
  }
  