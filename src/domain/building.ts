import {AggregateRoot} from "../core/domain/AggregateRoot";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import {BuildingDimensions} from "./buildingDimensions";

interface BuildingProps {
  code: string;
  dimensions: BuildingDimensions;
  name?: string;
  description?: string;
}

export class Building extends AggregateRoot<BuildingProps> {

  public constructor (props: BuildingProps, id?: UniqueEntityID) {
    super(props, id);
  }

  get id (): UniqueEntityID {
    return this._id;
  }

  get name() : string {
    return this.props.name;
  }

  get description() : string {
    return this.props.description;
  }

}
