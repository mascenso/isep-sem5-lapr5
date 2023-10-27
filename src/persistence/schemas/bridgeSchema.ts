import mongoose from 'mongoose';
import {IBridgePersistence} from "../../dataschema/IBridgePersistence";

const BridgeSchema = new mongoose.Schema(
  {
    domainId: { type: String, unique: true },
    code: { type: String, unique: true },
    name: { type: String, unique: true },
    floorA: { type: String, unique: false },
    floorB: { type: String, unique: false },
    buildingA: { type: String, unique: false },
    buildingB: { type: String, unique: false },
  },
  {
    timestamps: true
  }
);

export default mongoose.model<IBridgePersistence & mongoose.Document>('Bridge', BridgeSchema);
