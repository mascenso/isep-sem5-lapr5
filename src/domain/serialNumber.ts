import { ValueObject } from "../core/domain/ValueObject";
import { Result } from "../core/logic/Result";
import { Guard } from "../core/logic/Guard";
import { UniqueEntityID } from "../core/domain/UniqueEntityID";

interface RobotSerialNumberProps {
    value: string;
}

export class SerialNumber extends ValueObject<RobotSerialNumberProps> {

    get value(): string {
        return this.props.value;
    }


    private constructor(props: RobotSerialNumberProps) {
        super(props);
    }

    public static create(serialNumber: string): Result<SerialNumber> {
        const guardResult = Guard.againstNullOrUndefined(serialNumber, 'serialNumber');

        if (!guardResult.succeeded) {
            return Result.fail<SerialNumber>(guardResult.message);
        } else {
            const idPattern = /^[A-Za-z0-9\s]+$/;

            if (serialNumber && !serialNumber.toString().match(idPattern)) {

                return Result.fail<SerialNumber>("The Robot Serial Number must be alphanumeric and can contain spaces.");

            } else if (serialNumber.length > 50) {

                return Result.fail<SerialNumber>("The maximum length of the Serial Number is 50 characters");

            }
            const uniqueID = new UniqueEntityID(serialNumber);
            return Result.ok<SerialNumber>(new SerialNumber({ value: serialNumber }))
        }
    }
}
