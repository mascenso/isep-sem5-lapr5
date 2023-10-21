export interface IBuildingPersistence {
  _id: string;
  code: string;
  dimensions: {maxWidth: number, maxLength: number};
  name: string;
  description: string;
  salt: string;
}
