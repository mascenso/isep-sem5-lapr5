import { AggregateRoot } from "../core/domain/AggregateRoot";
import { UniqueEntityID } from "../core/domain/UniqueEntityID";
import { Result } from "../core/logic/Result";
import { BridgeId } from "./bridgeId";
import IBridgeDTO from "../dto/IBridgeDTO";

interface BridgeProps {
  code: string;
  name: string;
  floorAId: string;
  floorBId: string;
  buildingAId?: string;
  buildingBId?: string;
}

export class Bridge extends AggregateRoot<BridgeProps> {

  get id (): UniqueEntityID {
    return this._id;
  }

  get name() : string {
    return this.props.name;
  }

  get code() : string {
    return this.props.code;
  }

  get floorAId() : string {
    return this.props.floorAId;
  }

  get floorBId() : string {
    return this.props.floorBId;
  }

  get buildingAId() : string {
    return this.props.buildingAId;
  }

  get buildingBId() : string {
    return this.props.buildingBId;
  }

  set name ( value: string) {
    this.props.name = value;
  }

  set code ( value: string) {
    this.props.code = value;
  }

  set floorAId (value: string) {
    this.props.floorAId = value;
  }

  set floorBId (value: string) {
    this.props.floorBId = value;
  }

  set buildingAId (value: string) {
    this.props.buildingAId = value;
  }

  set buildingBId (value: string) {
    this.props.buildingBId = value;
  }

  private constructor (props: BridgeProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create (bridgeDTO: IBridgeDTO, id?: UniqueEntityID): Result<Bridge> {
    const name = bridgeDTO.name;
    const code = bridgeDTO.code;
    const floorAId = bridgeDTO.floorAId;
    const floorBId = bridgeDTO.floorBId;


    if (!!name === false || name.length === 0) {
      return Result.fail<Bridge>('Must provide a bridge name')
    } else {
      const bridge = new Bridge({
        name: name,
        code: code,
        floorAId: floorAId,
        floorBId: floorBId
      }, id);
      return Result.ok<Bridge>( bridge )
    }
  }
}
