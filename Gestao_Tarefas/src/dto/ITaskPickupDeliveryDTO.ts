import { User } from "../domain/task-agg/user";

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
  user:User;
  deliveryContact:User;
  pickupContact:User;
  approved:boolean;
  pending:boolean;
  planned:boolean;
}
