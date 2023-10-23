import { Router } from 'express';
import { celebrate, Joi } from 'celebrate';
import { Container } from 'typedi';
import config from "../../../config";
import IBuildingController from "../../controllers/IControllers/IBuildingController";

const route = Router();

export default (app: Router) => {
  app.use('/buildings', route);

  const ctrl = Container.get(config.controllers.building.name) as IBuildingController;

  route.post('',
    celebrate({
      body: Joi.object({
        code: Joi.string().required(),

          maxWidth: Joi.number().required(),
          maxLength: Joi.number().required(),

        name: Joi.string(),
        description: Joi.string()
      })
    }),
    (req, res, next) => ctrl.createBuilding(req, res, next) );

  route.patch('',
    celebrate({
      body: Joi.object({
        id: Joi.string().required(),
        code: Joi.string(),
        maxWidth: Joi.number(),
        maxLength: Joi.number(),
        name: Joi.string(),
        description: Joi.string()
      })
    }),
    (req, res, next) => ctrl.updateBuilding(req, res, next) );

  route.put('',
  celebrate({
    body: Joi.object({
      id: Joi.string().required(),
      code: Joi.string().required(),
      maxWidth: Joi.number().required(),
      maxLength: Joi.number().required(),
      name: Joi.string().required(),
      description: Joi.string().required()
    })
  }),
  (req, res, next) => ctrl.updateBuilding(req, res, next) );

  route.get('', 
    (req, res, next) => { ctrl.getAllBuildings(req, res, next);
  });

  route.get('/buildings/minmaxfloors', async (req, res, next) => {
    const { minFloors, maxFloors } = req.query;
  
    try {
      const result = await ctrl.getBuildingsByMinMaxFloors(req, res, next);
  
      if (result.isFailure) {
        return res.status(402).json('Error retrieving buildings').send();
      }
  
      let filteredBuildings = result.getValue();
  
      if (minFloors) {
        filteredBuildings = filteredBuildings.filter(building => building.floors >= minFloors);
      }
  
      if (maxFloors) {
        filteredBuildings = filteredBuildings.filter(building => building.floors <= maxFloors);
      }
  
      res.json(filteredBuildings);
    } catch (error) {
      next(error);
    }
  });
  
  


};
