export interface BridgeResponseDTO {
  id: string;
  name: string;
  code : string;
  floorAId : string;
  floorBId : string;
}

export interface CreateBridgeRequestDTO {
  name: string;
  code : string;
  floorAId : string;
  floorBId : string;
}

export interface BridgeDTO {
  name: string;
  code : string;
  floorAId : string;
  floorBId : string;
}


