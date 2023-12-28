import { Task } from "./task";

export interface TaskVigilance extends Task {
    buildingId: string;
    floors: object[];
    contactNumber: number;
    approved: boolean;
    pending: boolean;
    planned: boolean;
}