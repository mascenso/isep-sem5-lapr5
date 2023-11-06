import { IFloorDTO } from "./IFloorDTO";

export default interface IBuildingBridgeDTO {
  floor: IFloorDTO;
  connectedFloors: IFloorDTO[];
}
