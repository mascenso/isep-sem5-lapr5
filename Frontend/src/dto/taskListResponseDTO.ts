export interface TaskListResponseDTO {
  id: string;
  description: string;
  buildingId?: string;
  floors?: object[];
  pickupLocalization?: any;
  deliveryLocalization?: any;
  contactNumber:number;
  user:any;
  deliveryContact?:any;
  pickupContact?:any;
  startPosition?: number[];
  endPosition?: number[];
  taskStatus:{
    approved:boolean;
    pending:boolean;
    planned :boolean;
  };
  taskType:string;
}
