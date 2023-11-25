export interface BridgeResponseDTO {
  id: string;
  name: string;
  code : string;
  floorAId : string;
  floorBId : string;
  buildingAId: string;
  buildingBId: string;
}

export interface CreateBridgeRequestDTO {
  name: string;
  code : string;
  floorAId : string;
  floorBId : string;
  buildingAId: string;
  buildingBId: string;
}

export interface BridgeDTO {
  name: string;
  code : string;
  floorAId : string;
  floorBId : string;
  buildingAId: string;
  buildingBId: string;
}

export interface BridgeFloorBuildingDTO {
  name: string;
  code : string;
  floorNumberA: number;
  buildingNameA: string;
  floorNumberB: number;
  buildingNameB: string;
}



