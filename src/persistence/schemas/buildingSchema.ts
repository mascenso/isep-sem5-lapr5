import mongoose from 'mongoose';
import {IBuildingPersistence} from "../../dataschema/IBuildingPersistence";

const BuildingSchema = new mongoose.Schema(
  {
    domainId: { type: String, unique: true },
    code: { type: String, unique: true },
    name: { type: String, unique: false },
    description: { type: String, unique: false },
    dimensions: {maxWidth: Number, maxLength: Number}
  },
  {
    timestamps: true
  }
);

export default mongoose.model<IBuildingPersistence & mongoose.Document>('Building', BuildingSchema);
