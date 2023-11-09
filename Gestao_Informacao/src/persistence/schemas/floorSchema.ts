import mongoose from 'mongoose';
import {IFloorPersistence} from "../../dataschema/IFloorPersistence";

const FloorSchema = new mongoose.Schema(
  {
    domainId: { type: String, unique: true },
    buildingId: { type: String, unique: false },
    floorNumber: { type: Number, unique: false },
    description: { type: String, unique: false },
    width: {type: Number, unique: false},
    length: {type: Number, unique: false},
    floorMap: { type: [[Number]], unique: false } //array
  },
  {
    timestamps: true
  }
);

export default mongoose.model<IFloorPersistence & mongoose.Document>('Floor', FloorSchema);
