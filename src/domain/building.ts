import {AggregateRoot} from "../core/domain/AggregateRoot";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import {Result} from "../core/logic/Result";
import {Guard} from "../core/logic/Guard";

interface BuildingProps {
  code: string;
  dimensions: {maxWidth: number, maxLength: number};
  name?: string;
  description?: string;
}

export class Building extends AggregateRoot<BuildingProps> {

  private constructor (props: BuildingProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create (props: BuildingProps, id?: UniqueEntityID): Result<Building> {

    const guardedProps = [
      { argument: props.code, argumentName: 'code' },
      { argument: props.dimensions, argumentName: 'dimensions' },
      { argument: props.name, argumentName: 'name' },
      { argument: props.description, argumentName: 'description' }
    ];

    const guardResult = Guard.againstNullOrUndefinedBulk(guardedProps);

    if (!guardResult.succeeded) {
      return Result.fail<Building>(guardResult.message)
    }
    else {
      const building = new Building({
        ...props
      }, id);

      return Result.ok<Building>(building);
    }
  }

  get id (): UniqueEntityID {
    return this._id;
  }

  get name() : string {
    return this.props.name;
  }

  get code() : string {
    return this.props.code;
  }

  get description() : string {
    return this.props.description;
  }

}
