import { TaskViewModel } from "./taskView";

export interface TaskVigilanceViewModel extends TaskViewModel {
    buildingId: string;
    floors: Floor[];
    startPosition: number[];
    endPosition: number[];
    contactNumber: number;
    approved: boolean;
    pending: boolean;
    planned: boolean;
  }

  export interface Floor {
    description: string;
    floorId: string;
    floorNumber: number;
  }