
export interface TaskVigilanceRequestDTO {
  id: string;
  description: string;
  buildingId: string;
  floors: object[];
  contactNumber: number;
  user:object;
  approved: boolean;
}
