import { AggregateRoot } from "../core/domain/AggregateRoot";
import { UniqueEntityID } from "../core/domain/UniqueEntityID";
import { Result } from "../core/logic/Result";
import { BridgeId } from "./bridgeId";
import IBridgeDTO from "../dto/IBridgeDTO";


interface BridgeProps {
  code: string;
  name: string;
  floorA: string;
  floorB: string;
  //bridgedFloors: string[];
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

  /*
  get bridgedFloors() : string[] {
    return this.props.bridgedFloors;
  }

  set bridgedFloors (value: string[]) {
    this.props.bridgedFloors = value.sort();
  }
   */

  set floorA (value: string) {
    this.props.floorA = value;
  }

  set floorB (value: string) {
    this.props.floorB = value;
  }

  private constructor (props: BridgeProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create (roleDTO: IBridgeDTO, id?: UniqueEntityID): Result<Bridge> {
    const name = roleDTO.name;
    const code = roleDTO.code;
    const floorA = roleDTO.floorA;
    const floorB = roleDTO.floorB;
    //const bridgedFloors = roleDTO.bridgedFloors;

    if (!!name === false || name.length === 0) {
      return Result.fail<Bridge>('Must provide a role name')
    } else {
      const role = new Bridge({
        name: name,
        code: code,
        //bridgedFloors : bridgedFloors,
        floorA: floorA,
        floorB: floorB
      }, id);
      return Result.ok<Bridge>( role )
    }
  }

  /*
  public static create (props: BridgeProps, id?: UniqueEntityID): Result<Bridge> {
    const guardedProps = [
      { argument: props.code, argumentName: 'code' },
      { argument: props.name, argumentName: 'name' },
      { argument: props.bridgedFloors, argumentName: 'bridgedFloors' },
    ];

    const guardResult = Guard.againstNullOrUndefinedBulk(guardedProps);

    if (!guardResult.succeeded) {
      return Result.fail<Bridge>(guardResult.message)
    }
    else {
      const building = new Bridge({
        ...props
      }, id);

      return Result.ok<Bridge>(building);
    }
  }

   */


}
