export interface IBuildingPersistence {
  _id: string;
  code: string;
  maxWidth: number; 
  maxLength: number;
  name: string;
  description: string;
  salt: string;
}
