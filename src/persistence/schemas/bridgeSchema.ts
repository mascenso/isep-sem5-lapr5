import mongoose from 'mongoose';
import {IBridgePersistence} from "../../dataschema/IBridgePersistence";

const BridgeSchema = new mongoose.Schema(
  {
    domainId: { type: String, unique: true },
    code: { type: String, unique: true },
    name: { type: String, unique: true },
    floorAId: { type: String, unique: false },
    floorBId: { type: String, unique: false },
    buildingAId: { type: String, unique: false },
    buildingBId: { type: String, unique: false },
  },
  {
    timestamps: true
  }
);

export default mongoose.model<IBridgePersistence & mongoose.Document>('Bridge', BridgeSchema);
