
export interface IRoomPersistence {
  _id: string;
  buildingId: string;
  floorId: string;
  name: string;
  roomType: string;
  description: string;
  salt: string;
}
