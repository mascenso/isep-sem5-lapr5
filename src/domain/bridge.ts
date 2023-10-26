import { AggregateRoot } from "../core/domain/AggregateRoot";
import { UniqueEntityID } from "../core/domain/UniqueEntityID";
import { Result } from "../core/logic/Result";
import { BridgeId } from "./bridgeId";
import IBridgeDTO from "../dto/IBridgeDTO";
import { BridgedFloors } from "./bridgedFloors";

interface BridgeProps {
  code: string;
  name: string;
  floorA: string;
  floorB: string;
  buildingA: string;
  buildingB: string;
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

  get buildingA() : string {
    return this.props.buildingA;
  }

  get buildingB() : string {
    return this.props.buildingB;
  }

  get floorA() : string {
    return this.props.floorA;
  }

  get floorB() : string {
    return this.props.floorB;
  }

  set name ( value: string) {
    this.props.name = value;
  }

  set code ( value: string) {
    this.props.code = value;
  }

  set floorA (value: string) {
    this.props.floorA = value;
  }

  set floorB (value: string) {
    this.props.floorB = value;
  }

  set buildingA (value: string) {
    this.props.buildingA = value;
  }

  set buildingB (value: string) {
    this.props.buildingB = value;
  }

  private constructor (props: BridgeProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create (roleDTO: IBridgeDTO, id?: UniqueEntityID): Result<Bridge> {
    const name = roleDTO.name;
    const code = roleDTO.code;
    const floorA = roleDTO.floorA;
    const floorB = roleDTO.floorB;
    const buildingA = roleDTO.buildingA;
    const buildingB = roleDTO.buildingB;

    if (!!name === false || name.length === 0) {
      return Result.fail<Bridge>('Must provide a bridge name')
    } else {
      const bridge = new Bridge({
        name: name,
        code: code,
        buildingA: buildingA,
        buildingB: buildingB,
        floorA: floorA,
        floorB: floorB
      }, id);
      return Result.ok<Bridge>( bridge )
    }
  }
}
