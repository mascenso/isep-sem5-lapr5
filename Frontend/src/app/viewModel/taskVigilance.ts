import { TaskViewModel } from "./taskView";

export class TaskVigilanceViewModel extends TaskViewModel {

    buildingId: string;
    floors: object[]; 
    contactNumber: number;
    approved: boolean;
    pending: boolean;
    planned: boolean;

    constructor(data: any) {
        super(data); 
        this.buildingId = data.buildingId;
        this.floors = data.floors;
        this.contactNumber = data.contactNumber;
        this.approved = data.approved;
        this.pending = data.pending;
        this.planned = data.planned;

    }
    
}