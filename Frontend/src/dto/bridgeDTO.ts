export interface BridgeDTO {
  id: string;
  name: string;
  code : string;
  floorAId : string;
  floorBId : string;
  buildingAId: string;
  buildingBId: string;
  floorNumberA?: number;
  buildingNameA?: string;
  floorNumberB?: number;
  buildingNameB?: string;
}

export interface BridgeUpdateDTO {
  name?: string;
  code? : string;
  floorAId? : string;
  floorBId? : string;
}



