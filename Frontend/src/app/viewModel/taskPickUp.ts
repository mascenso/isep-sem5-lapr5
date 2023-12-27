import { TaskViewModel } from "./taskView";

export class TaskPickupViewModel extends TaskViewModel {
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



    constructor(data: any) {
      super(data); 
      this.pickupLocalization = data.pickupLocalization;
      this.deliveryLocalization = data.deliveryLocalization;
      this.contactNumber = data.contactNumber;
      this.deliveryContact = data.deliveryContact;
      this.pickupContact = data.pickupContact;
      this.approved = data.approved;
      this.pending = data.pending;
      this.planned=data.planned

    }
  }