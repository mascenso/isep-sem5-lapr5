import {ValueObject} from "../core/domain/ValueObject";

interface BuildingDimensionsProps {
  maxWidth: number;
  maxLength: number;
}

export class BuildingDimensions extends ValueObject<BuildingDimensionsProps> {

  get width(): number {
    return this.props.maxWidth;
  }

  get length(): number {
    return this.props.maxLength;
  }

  public constructor (props: BuildingDimensionsProps) {
    super(props);
  }

}
