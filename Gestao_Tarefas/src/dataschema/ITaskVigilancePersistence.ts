
export interface ITaskVigilancePersistence {
  domainId: string;
  description: string;
  buildingId: string;
  floors: object[];
  contactNumber: number;
  user:object;
  approved:boolean;
  pending:boolean;
  planned:boolean;
}
