
export interface TaskPickupRequestDTO {
  id: string;
  description: string;
  pickupLocalization: {
    buildingId:String;
    floor:object;
    room: number[];
  };
  deliveryLocalization:{
    buildingId:String;
    floor:object;
    room: number[];
  };
  contactNumber:number;
  user:object;
  deliveryContact:{
    name:String;
    contactNumber:number;
  };
  pickupContact:{
    name:String;
    contactNumber:number;
  };
  taskStatus:{
  approved:boolean;
  pending:boolean;
  planned :boolean;
  };
}


export interface TaskPickupResponseDTO {
  id: string;
  description: string;
  pickupLocalization: {
    buildingId:String;
    floor:object;
    room: number[];
  };
  deliveryLocalization:{
    buildingId:String;
    floor:object;
    room: number[];
  };
  contactNumber:number;
  user:object;
  deliveryContact:{
    name:String;
    contactNumber:number;
  };
  pickupContact:{
    name:String;
    contactNumber:number;
  };
  approved:boolean;
  pending:boolean;
  planned :boolean;
}
