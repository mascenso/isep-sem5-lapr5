import { TaskViewModel } from "./taskView";

export interface TaskPickupViewModel extends TaskViewModel {
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