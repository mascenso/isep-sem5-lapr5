export default interface IBridgeDTO {
        id: string;
        code: string;
        name: string;
        floorAId: string;
        floorBId: string;
        buildingAId: string;
        buildingBId: string;
}


export default interface IBridgeResponseDTO {
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
