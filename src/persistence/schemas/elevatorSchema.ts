import mongoose from 'mongoose';
import {IElevatorPersistence} from "../../dataschema/IElevatorPersistence";

const ElevatorSchema = new mongoose.Schema(
  {
    domainId: { type: String, unique: true },
    code: { type: String, unique: true },
    coordX: { type: Number, unique: false },
    coordY: { type: Number, unique: false }
  },
  {
    timestamps: true
  }
);

export default mongoose.model<IElevatorPersistence & mongoose.Document>('Elevator', ElevatorSchema);
