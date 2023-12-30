import { TaskViewModel } from "./taskView";

export interface TaskVigilanceViewModel extends TaskViewModel {
    buildingId: string;
    floors: object[];
    startPosition: number[];
    endPosition: number[];
    contactNumber: number;
    approved: boolean;
    pending: boolean;
    planned: boolean;
  }