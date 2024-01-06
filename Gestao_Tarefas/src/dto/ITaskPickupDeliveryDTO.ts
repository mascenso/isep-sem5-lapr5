import { User } from "../domain/task-agg/user";
import { LocationRoom } from "../domain/task-agg/locationRoom";

export default interface ITaskPickupDeliveryDTO {
  id: string;
  description: string;
  pickupLocalization: LocationRoom;
  deliveryLocalization: LocationRoom;
  contactNumber:number;
  user:User;
  deliveryContact:User;
  pickupContact:User;
  approved:boolean;
  pending:boolean;
  planned:boolean;
}
