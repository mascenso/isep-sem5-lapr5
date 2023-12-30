
export interface ITaskVigilancePersistence {
  domainId: string;
  description: string;
  buildingId: string;
  floors: object[];
  startPosition: number[];
  endPosition: number[];
  contactNumber: number;
  user:object;
  approved:boolean;
  pending:boolean;
  planned:boolean;
}
