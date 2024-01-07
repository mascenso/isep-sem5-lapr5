import {LocationRoom} from "../domain/task-agg/locationRoom";
import {User} from "../domain/task-agg/user";

export default interface ITaskSearchResponseDTO {
  id: string;
  description: string;
  buildingId?: string;
  floors?: object[];
  pickupLocalization?: LocationRoom;
  deliveryLocalization?: LocationRoom;
  contactNumber:number;
  user:User;
  deliveryContact?:User;
  pickupContact?:User;
  startPosition?: number[];
  endPosition?: number[];
  taskStatus:object;
  taskType: TaskType;
}

export enum TaskType {
  VIGILANCE = "Vigilance",
  DELIVERY = "Pickup"
}

