export interface IBuildingPersistence {
  _id: string;
  domainId:string;
  code: string;
  maxWidth: number; 
  maxLength: number;
  name: string;
  description: string;
  salt: string;
}
