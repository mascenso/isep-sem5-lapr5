
export default interface ITaskPickupDeliveryDTO {
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
}
