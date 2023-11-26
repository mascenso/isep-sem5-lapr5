export interface BridgeResponseDTO {
  id: string;
  code: string;
  name: string;
  floorAId: string;
  floorANumber: number;
  floorBId: string;
  floorBNumber: number;
  buildingAId: string;
  buildingAName: string;
  buildingBId: string;
  buildingBName: string;
}

export interface BridgeRequestDTO {
  name?: string;
  code? : string;
  floorAId? : string;
  floorBId? : string;
}



