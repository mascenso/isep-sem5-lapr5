
export default interface ITaskVigilanceDTO {
  id: string;
  description: string;
  buildingId: string;
  floors: object[];
  contactNumber: number;
  user:object;
  approved: boolean;
}
