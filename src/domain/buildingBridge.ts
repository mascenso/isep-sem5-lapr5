import { AggregateRoot } from "../core/domain/AggregateRoot";
import { UniqueEntityID } from "../core/domain/UniqueEntityID";
import { Result } from "../core/logic/Result";
import { BuildingBridgeId } from "./buildingBridgeId";
import IBuildingBridgeDTO from "../dto/IBuildingBridgeDTO";
import { Guard } from "../core/logic/Guard";


interface BuildingBridgeProps {
    buildingName: string,
    floorNumber: number,
    description: string
}

export class BuildingBridge extends AggregateRoot<BuildingBridgeProps> {

    private constructor(props: BuildingBridgeProps, id?: UniqueEntityID) {
        super(props, id);
    }

    public static create(props: BuildingBridgeProps, id?: UniqueEntityID): Result<BuildingBridge> {

        const guardedProps = [
            { argument: props.floorNumber, argumentName: 'floorNumber' },
            { argument: props.buildingName, argumentName: 'buildingName' },
            { argument: props.description, argumentName: 'description' }
        ];

        const guardResult = Guard.againstNullOrUndefinedBulk(guardedProps);

        if (!guardResult.succeeded) {
            return Result.fail<BuildingBridge>(guardResult.message)
        }
        else {
            const building = new BuildingBridge({
                ...props
            }, id);

            return Result.ok<BuildingBridge>(building);
        }
    }

    get id(): UniqueEntityID {
        return this._id;
    }

    get floorNumber(): number {
        return this.props.floorNumber;
    }

    set floorNumber(value: number) {
        this.props.floorNumber = value;
    }

    get buildingName(): string {
        return this.props.buildingName;
    }

    set buildingName(value: string) {
        this.props.buildingName = value;
    }

    get description(): string {
        return this.props.description;
    }

    set description(value: string) {
        this.props.description = value;
    }

    
}
