import { Task } from "./task";

export interface TaskVigilance extends Task {
    buildingId: string;
    floors: object[];
    startPosition: number[];
    endPosition: number[];
    contactNumber: number;
    approved: boolean;
    pending: boolean;
    planned: boolean;
}