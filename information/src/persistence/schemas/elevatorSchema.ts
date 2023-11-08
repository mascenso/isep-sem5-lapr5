import mongoose from 'mongoose';
import {IElevatorPersistence} from "../../dataschema/IElevatorPersistence";

const ElevatorSchema = new mongoose.Schema(
  {
    domainId: { type: String, unique: true },
    code: { type: String, unique: true },
    floorId: { type: String, unique: false },
    coordX1: { type: Number, unique: false },
    coordY1: { type: Number, unique: false },
    coordX2: { type: Number, unique: false },
    coordY2: { type: Number, unique: false }
  },
  {
    timestamps: true
  }
);

export default mongoose.model<IElevatorPersistence & mongoose.Document>('Elevator', ElevatorSchema);
