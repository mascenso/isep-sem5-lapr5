
export interface ITaskPickupDeliveryPersistence {
  domainId: string;
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
  pickupDelivery:{
    name:String;
    contactNumber:number;
  }
}
