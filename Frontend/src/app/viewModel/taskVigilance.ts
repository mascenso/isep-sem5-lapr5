import { TaskViewModel } from "./taskView";

export interface TaskVigilanceViewModel extends TaskViewModel {
    buildingId: string;
    floors: object[];
    contactNumber: number;
    approved: boolean;
    pending: boolean;
    planned: boolean;
  }