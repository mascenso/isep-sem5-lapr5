import { Task } from "./task";

 export interface TaskPickup extends Task {
   pickupLocalization: {
     buildingId: string;
     floor: object;
     room: number[];
   };
   deliveryLocalization: {
     buildingId: string;
     floor: object;
     room: number[];
   };
   contactNumber: number;
   deliveryContact: {
     name: string;
     contactNumber: number;
   };
   pickupContact: {
     name: string;
     contactNumber: number;
   };
   approved: boolean;
   pending: boolean;
   planned: boolean;
 }