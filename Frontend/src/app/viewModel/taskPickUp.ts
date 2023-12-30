import { TaskViewModel } from "./taskView";

export interface TaskPickupViewModel extends TaskViewModel {
  pickupLocalization: {
    buildingId: string;
    floor: Floor;
    room: number[];
  };
  deliveryLocalization: {
    buildingId: string;
    floor: Floor;
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

export interface Floor {
  floorNumber: string;
  code: string;
}